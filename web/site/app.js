"use strict";

// Waits for the wasm module's async instantiation to finish and set
// globalThis.Nsdl -- there's no exported promise/callback to await this
// directly (see the WebAssembly build README section), so this polls,
// same as the Node smoke test that verified this module.
function waitForNsdl() {
  return new Promise((resolve) => {
    (function poll() {
      if (globalThis.Nsdl) return resolve(globalThis.Nsdl);
      setTimeout(poll, 30);
    })();
  });
}

let world = null;
let nextFileId = 0;

const el = (id) => document.getElementById(id);

function logConsole(kind, text) {
  const line = document.createElement("div");
  line.className = "line " + kind;
  line.textContent = text;
  const box = el("console-log");
  box.appendChild(line);
  box.scrollTop = box.scrollHeight;
}

// ------------------------------------------------------------------
// Scenario panel: preset picker + editable per-file source textareas.
// ------------------------------------------------------------------

function addFileEditor(name, content) {
  const id = "file-" + nextFileId++;
  const wrap = document.createElement("div");
  wrap.className = "file-editor";
  wrap.dataset.id = id;

  const head = document.createElement("div");
  head.className = "file-editor-head";

  const label = document.createElement("label");
  label.textContent = name;
  label.htmlFor = id;

  const remove = document.createElement("button");
  remove.type = "button";
  remove.className = "small";
  remove.textContent = "remove";
  remove.onclick = () => wrap.remove();

  head.appendChild(label);
  head.appendChild(remove);

  const textarea = document.createElement("textarea");
  textarea.id = id;
  textarea.dataset.filename = name;
  textarea.value = content;
  textarea.spellcheck = false;

  wrap.appendChild(head);
  wrap.appendChild(textarea);
  el("file-editors").appendChild(wrap);
}

function clearFileEditors() {
  el("file-editors").innerHTML = "";
}

function gatherSources() {
  return Array.from(document.querySelectorAll("#file-editors textarea")).map((t) => ({
    name: t.dataset.filename,
    content: t.value,
  }));
}

async function loadPreset(preset) {
  clearFileEditors();
  el("preset-hint").textContent = preset.hint || "";
  for (const name of preset.files) {
    const res = await fetch("examples/" + name);
    if (!res.ok) {
      logConsole("error", "failed to fetch examples/" + name + " (" + res.status + ")");
      continue;
    }
    addFileEditor(name, await res.text());
  }
}

async function loadManifest() {
  const res = await fetch("examples/manifest.json");
  const presets = await res.json();
  const select = el("preset-select");
  presets.forEach((preset, i) => {
    const opt = document.createElement("option");
    opt.value = String(i);
    opt.textContent = preset.label;
    select.appendChild(opt);
  });
  select.onchange = () => loadPreset(presets[Number(select.value)]);
  if (presets.length > 0) await loadPreset(presets[0]);
}

function setLoadStatus(ok, text) {
  const p = el("load-status");
  p.textContent = text;
  p.className = "status " + (ok ? "ok" : "err");
}

function doLoadWorld() {
  const sources = gatherSources();
  if (sources.length === 0) {
    setLoadStatus(false, "no source files to load");
    return;
  }
  try {
    world = Nsdl.loadSources(sources);
    setLoadStatus(true, "world loaded (" + sources.length + " file" + (sources.length === 1 ? "" : "s") + ")");
    logConsole("ack", "=== world (re)loaded from " + sources.map((s) => s.name).join(", ") + " ===");
    el("console-input").disabled = false;
    el("console-form").querySelector("button").disabled = false;
    const st = world.state();
    buildTopologySvg(st.instances, st.connections);
    refreshState();
  } catch (e) {
    world = null;
    setLoadStatus(false, "failed to load: " + describeError(e));
  }
}

// wasm exceptions don't stringify usefully (`[object WebAssembly.Exception]`)
// -- OCaml's own Lex_error/Parser.Error messages don't survive the
// wasm<->JS boundary as readable text, so this at least names *which*
// kind of failure it was rather than showing an opaque object.
function describeError(e) {
  const s = String(e);
  if (s.indexOf("WebAssembly") !== -1) return "invalid input (parse or lex error) -- check the source syntax";
  return s;
}

// ------------------------------------------------------------------
// Topology diagram: instances as nodes, connections as edges, laid out
// with a small force simulation (repulsion + spring edges + centering).
// world.connections never changes once a world is loaded -- Sim records
// it at scenario-load time and disconnect/reconnect only change the
// medium's physical_state field, not the topology itself (unplugging a
// cable doesn't remove the cable) -- so the layout is computed once per
// world load (buildTopologySvg, called from doLoadWorld) and every
// subsequent refresh only re-styles the existing elements
// (updateTopologyStyling, called from refreshState): edge color/dash
// from the medium's live physical_state, node label from the instance's
// live lifecycle state. Nothing moves just because you ran a command.
// ------------------------------------------------------------------

const SVG_NS = "http://www.w3.org/2000/svg";
const TOPOLOGY_W = 900;
const TOPOLOGY_H = 300;

let topology = null; // { edges: [{a,b,aPath,bPath,medium}], edgeEls: [line], nodeEls: Map(name -> {circle,stateText,title}) }
let lastInstances = []; // latest world.state().instances, refreshed every refreshState() -- click handlers read from this

// "switch.port[2]" / "workstation.eth0" -> the leading instance name --
// same convention Sim.split_path uses for connection endpoints.
function baseName(path) {
  return path.split(".")[0];
}

function computeForceLayout(instances, connections) {
  const nodes = instances.map((inst) => ({
    name: inst.name,
    x: TOPOLOGY_W / 2 + (Math.random() - 0.5) * 80,
    y: TOPOLOGY_H / 2 + (Math.random() - 0.5) * 80,
    vx: 0,
    vy: 0,
  }));
  const byName = new Map(nodes.map((n) => [n.name, n]));
  const edges = connections
    .map((c) => ({ a: baseName(c.a), b: baseName(c.b), aPath: c.a, bPath: c.b, medium: c.medium }))
    .filter((e) => byName.has(e.a) && byName.has(e.b));

  const REPULSION = 9000;
  const SPRING_LEN = 170;
  const SPRING_K = 0.02;
  const CENTER_K = 0.01;
  const DAMPING = 0.82;

  for (let iter = 0; iter < 500; iter++) {
    for (let i = 0; i < nodes.length; i++) {
      for (let j = i + 1; j < nodes.length; j++) {
        const a = nodes[i];
        const b = nodes[j];
        let dx = a.x - b.x;
        let dy = a.y - b.y;
        const distSq = Math.max(dx * dx + dy * dy, 1);
        const dist = Math.sqrt(distSq);
        const force = REPULSION / distSq;
        dx /= dist;
        dy /= dist;
        a.vx += dx * force;
        a.vy += dy * force;
        b.vx -= dx * force;
        b.vy -= dy * force;
      }
    }
    for (const e of edges) {
      const a = byName.get(e.a);
      const b = byName.get(e.b);
      let dx = b.x - a.x;
      let dy = b.y - a.y;
      const dist = Math.max(Math.sqrt(dx * dx + dy * dy), 1);
      const force = (dist - SPRING_LEN) * SPRING_K;
      dx /= dist;
      dy /= dist;
      a.vx += dx * force;
      a.vy += dy * force;
      b.vx -= dx * force;
      b.vy -= dy * force;
    }
    for (const n of nodes) {
      n.vx += (TOPOLOGY_W / 2 - n.x) * CENTER_K;
      n.vy += (TOPOLOGY_H / 2 - n.y) * CENTER_K;
      n.vx *= DAMPING;
      n.vy *= DAMPING;
      n.x = Math.min(TOPOLOGY_W - 50, Math.max(50, n.x + n.vx));
      n.y = Math.min(TOPOLOGY_H - 40, Math.max(40, n.y + n.vy));
    }
  }
  return { nodes, edges, byName };
}

function svgEl(name, attrs) {
  const node = document.createElementNS(SVG_NS, name);
  for (const k in attrs) node.setAttribute(k, attrs[k]);
  return node;
}

function shorten(s) {
  return s.length > 12 ? s.slice(0, 11) + "…" : s;
}

// Looks up a field on any instance (device or medium) from the latest
// world.state() snapshot. Returns null if either the instance or the field
// itself was never set -- callers decide their own "never touched" default
// (a medium's attached_a/attached_b/physical_state all default to
// "attached", same as Sim.side_state/medium_attached do internally).
function fieldValue(instName, key) {
  const inst = lastInstances.find((i) => i.name === instName);
  if (!inst) return null;
  const f = inst.fields.find((f) => f.key === key);
  return f ? f.value : null;
}

function instanceState(name) {
  const inst = lastInstances.find((i) => i.name === name);
  return inst ? inst.state : "";
}

// A device's own IP, for the node tooltip -- prefers the live, negotiated
// dhcp_address over a static declaration, else the first field whose value
// is plainly IP-shaped. Excludes both `VIdent "dhcp"` placeholders (no
// dot) and VRange's "A .. B" stringification (the ".." fails this pattern)
// without needing to know either of those Sim-side representations.
const IP_LIKE = /^\d{1,3}(\.\d{1,3}){3}(\/\d{1,2})?$/;
function bestIpField(inst) {
  const dhcp = inst.fields.find((f) => f.key === "dhcp_address");
  if (dhcp) return dhcp.value;
  const addr = inst.fields.find((f) => IP_LIKE.test(f.value));
  return addr ? addr.value : null;
}

// Toggles a device's power via the same console path a typed command
// would use -- "off" (or no lifecycle at all, the empty-string convention
// nsdl_web.ml's own module comment documents) tries to power on; anything
// else tries to power off. A type with neither handler just surfaces the
// normal invoke error in the console log, same as typing an unsupported
// command by hand -- no separate capability check.
function togglePower(name) {
  const state = instanceState(name);
  dispatch((state === "off" || state === "" ? "invoke power_on " : "invoke power_off ") + name);
}

// Toggles one specific endpoint (not the whole cable) via disconnect_endpoint
// /reconnect_endpoint, identified by that endpoint's own real port path.
function toggleEndpoint(portPath, mediumName, sideFieldKey) {
  const attached = (fieldValue(mediumName, sideFieldKey) || "attached") === "attached";
  dispatch((attached ? "disconnect_endpoint " : "reconnect_endpoint ") + portPath);
}

// Point on a node's own circle boundary (plus a small gap) facing the far
// node -- literally where a cable would meet that vertex. `fx`/`fy` is the
// far node's center.
function endpointPoint(nx, ny, fx, fy) {
  const dx = fx - nx;
  const dy = fy - ny;
  const dist = Math.max(Math.hypot(dx, dy), 1);
  const gap = 24 + 6;
  return { x: nx + (dx / dist) * gap, y: ny + (dy / dist) * gap };
}

function buildTopologySvg(instances, connections) {
  const svg = el("topology-svg");
  svg.innerHTML = "";
  topology = null;
  if (instances.length === 0) return;

  const { nodes, edges, byName } = computeForceLayout(instances, connections);

  const edgeEls = edges.map((e) => {
    const a = byName.get(e.a);
    const b = byName.get(e.b);
    const line = svgEl("line", { x1: a.x, y1: a.y, x2: b.x, y2: b.y, class: "edge" });
    line.appendChild(svgEl("title", {}));
    line.firstChild.textContent = e.medium;
    svg.appendChild(line);

    const label = svgEl("text", { x: (a.x + b.x) / 2, y: (a.y + b.y) / 2 - 5, class: "edge-label" });
    label.textContent = e.medium;
    svg.appendChild(label);

    return line;
  });

  // Two independently-clickable endpoint markers per edge, positioned
  // where the cable meets each vertex -- one per (portPath, sideFieldKey)
  // so a click always knows exactly which end and which of the medium's
  // two attached_a/attached_b fields it's toggling.
  const endpointEls = edges.map((e) => {
    const a = byName.get(e.a);
    const b = byName.get(e.b);
    const pa = endpointPoint(a.x, a.y, b.x, b.y);
    const pb = endpointPoint(b.x, b.y, a.x, a.y);
    const makeEndpoint = (p, portPath, sideFieldKey) => {
      const g = svgEl("g", { class: "endpoint", transform: "translate(" + p.x + "," + p.y + ")" });
      const hit = svgEl("circle", { class: "endpoint-hit", r: 12 });
      const dot = svgEl("circle", { class: "endpoint-marker", r: 6 });
      const title = svgEl("title", {});
      title.textContent = portPath;
      g.appendChild(hit);
      g.appendChild(dot);
      g.appendChild(title);
      g.addEventListener("click", () => toggleEndpoint(portPath, e.medium, sideFieldKey));
      svg.appendChild(g);
      return dot;
    };
    return {
      medium: e.medium,
      a: makeEndpoint(pa, e.aPath, "attached_a"),
      b: makeEndpoint(pb, e.bPath, "attached_b"),
    };
  });

  const nodeEls = new Map();
  for (const n of nodes) {
    const g = svgEl("g", { class: "node", transform: "translate(" + n.x + "," + n.y + ")" });
    const circle = svgEl("circle", { r: 24 });
    const nameText = svgEl("text", { class: "node-name", y: -30 });
    nameText.textContent = shorten(n.name);
    const stateText = svgEl("text", { class: "node-state", y: 4 });
    const title = svgEl("title", {});
    g.appendChild(circle);
    g.appendChild(nameText);
    g.appendChild(stateText);
    g.appendChild(title);
    g.addEventListener("click", () => togglePower(n.name));
    svg.appendChild(g);
    nodeEls.set(n.name, { circle, stateText, title });
  }

  topology = { edges, edgeEls, endpointEls, nodeEls };
}

function updateTopologyStyling(instances) {
  if (!topology) return;
  const byName = new Map(instances.map((i) => [i.name, i]));

  topology.endpointEls.forEach((ep) => {
    const mediumInst = byName.get(ep.medium);
    const sideDetached = (key) => {
      const f = mediumInst && mediumInst.fields.find((f) => f.key === key);
      return !!f && f.value !== "attached";
    };
    ep.a.classList.toggle("detached", sideDetached("attached_a"));
    ep.b.classList.toggle("detached", sideDetached("attached_b"));
  });

  for (const [name, { circle, stateText, title }] of topology.nodeEls) {
    const inst = byName.get(name);
    if (!inst) continue;
    stateText.textContent = shorten(inst.state || "");
    circle.classList.toggle("node-off", inst.state === "off" || inst.state === "");
    const ip = bestIpField(inst);
    const fieldsText = inst.fields.map((f) => f.key + "=" + f.value).join("\n");
    title.textContent =
      (ip ? "ip: " + ip + "\n" : "") +
      name +
      " : " +
      inst.type +
      (inst.state ? " (" + inst.state + ")" : "") +
      (fieldsText ? "\n" + fieldsText : "");
  }
}

// ------------------------------------------------------------------
// State panel: rendered from world.state() after every command.
// ------------------------------------------------------------------

// Fields prefixed like this are protocol-negotiated live state (leases,
// offers, ...), not a scenario's own static declaration -- e.g. a
// `clinic_client`'s `address = dhcp` field never changes once loaded, but
// its `dhcp_address` is what a real handshake actually installs (see
// lib/sim.ml's dhcp_discover/dispatch_message). Both are real fields;
// this is purely about making the negotiated ones easy to actually spot
// in a row that can otherwise have half a dozen fields in it.
function isLiveField(key) {
  return key.startsWith("dhcp_");
}

function renderInstances(instances) {
  const box = el("instances-table");
  if (instances.length === 0) {
    box.textContent = "(none)";
    return;
  }
  const table = document.createElement("table");
  table.className = "state-table";
  table.innerHTML = "<tr><th>name</th><th>type</th><th>state</th><th>fields</th></tr>";
  for (const inst of instances) {
    const row = document.createElement("tr");
    row.innerHTML =
      "<td>" + esc(inst.name) + "</td>" + "<td>" + esc(inst.type) + "</td>" + "<td>" + esc(inst.state || "—") + "</td>";

    const fieldsCell = document.createElement("td");
    fieldsCell.className = "fields";
    if (inst.fields.length === 0) {
      fieldsCell.textContent = "—";
    } else {
      for (const f of inst.fields) {
        const line = document.createElement("div");
        line.className = "field-row" + (isLiveField(f.key) ? " field-live" : "");
        const keySpan = document.createElement("span");
        keySpan.className = "field-key";
        keySpan.textContent = f.key;
        const valSpan = document.createElement("span");
        valSpan.className = "field-value";
        valSpan.textContent = f.value;
        line.appendChild(keySpan);
        line.appendChild(document.createTextNode(" = "));
        line.appendChild(valSpan);
        fieldsCell.appendChild(line);
      }
    }
    row.appendChild(fieldsCell);
    table.appendChild(row);
  }
  box.innerHTML = "";
  box.appendChild(table);
}

function renderList(id, items, empty) {
  const list = el(id);
  list.innerHTML = "";
  if (items.length === 0) {
    const li = document.createElement("li");
    li.textContent = empty;
    list.appendChild(li);
    return;
  }
  for (const text of items) {
    const li = document.createElement("li");
    li.textContent = text;
    list.appendChild(li);
  }
}

function esc(s) {
  const d = document.createElement("div");
  d.textContent = s;
  return d.innerHTML;
}

function refreshState() {
  if (!world) return;
  const st = world.state();
  lastInstances = st.instances;
  el("clock").textContent = "t = " + st.clock + "s";
  renderInstances(st.instances);
  renderList(
    "connections-list",
    st.connections.map((c) => c.a + " -> " + c.b + " via " + c.medium),
    "(none)"
  );
  renderList(
    "pending-list",
    st.pending.map((p) => "t=" + p.due + "s  " + p.label),
    "(none)"
  );
  renderList("log-list", st.log, "(empty)");
  updateTopologyStyling(st.instances);
}

// ------------------------------------------------------------------
// Console: same verb set bin/tui.ml already defines, plus the newer
// actions (dhcp/ping/print/thread_sight/packet_sight/disconnect/
// reconnect) that aren't wired into any TUI yet.
// ------------------------------------------------------------------

function dispatch(line) {
  const trimmed = line.trim();
  if (!trimmed) return;
  logConsole("input", "> " + trimmed);
  if (!world) {
    logConsole("error", "=> no world loaded -- click \"Load / reload world\" first");
    return;
  }
  const parts = trimmed.split(/\s+/);
  const verb = parts[0];
  let result;
  try {
    switch (verb) {
      case "inspect":
        result = world.inspect(parts[1]);
        break;
      case "set":
        result = world.configure(parts[1], parts.slice(2).join(" "));
        break;
      case "invoke":
        result = world.invoke(parts[1], parts[2]);
        break;
      case "advance": {
        const seconds = Nsdl.parseDuration(parts[1]);
        result = { kind: "ack", text: world.advance(seconds) };
        break;
      }
      case "power_cycle":
        result = world.powerCycle(parts[1]);
        break;
      case "restart_service":
        result = world.restartService(parts[1]);
        break;
      case "dhcp":
        result = world.dhcpDiscover(parts[1], parts[2], parts[3], parseFloat(parts[4]));
        break;
      case "ping": {
        const from = parts[1];
        const to = parts[2];
        const viaIdx = parts.indexOf("via");
        const via = viaIdx !== -1 ? parts[viaIdx + 1] : "";
        result = world.ping(from, to, via);
        break;
      }
      case "print":
        result = world.printJob(parts[1], parts[2], parts.slice(3).join(" "));
        break;
      case "thread_sight":
        result = world.threadSight(parts[1]);
        break;
      case "packet_sight":
        result = world.packetSight(parts[1], parts[2]);
        break;
      case "disconnect":
        result = world.disconnect(parts[1]);
        break;
      case "reconnect":
        result = world.reconnect(parts[1]);
        break;
      case "disconnect_endpoint":
        result = world.disconnectEndpoint(parts[1]);
        break;
      case "reconnect_endpoint":
        result = world.reconnectEndpoint(parts[1]);
        break;
      default:
        result = { kind: "error", text: "unrecognized command: " + verb };
    }
  } catch (e) {
    result = { kind: "error", text: describeError(e) };
  }
  logConsole(result.kind, "=> " + result.text);
  refreshState();
}

// ------------------------------------------------------------------

async function main() {
  logConsole("ack", "loading NSDL runtime...");
  await waitForNsdl();
  logConsole("ack", "runtime ready. pick a preset and click \"Load / reload world\".");

  await loadManifest();

  el("add-file").onclick = () => addFileEditor("new_file.nsdl", "");
  el("load-world").onclick = doLoadWorld;

  el("console-form").onsubmit = (ev) => {
    ev.preventDefault();
    const input = el("console-input");
    dispatch(input.value);
    input.value = "";
  };
}

main();
