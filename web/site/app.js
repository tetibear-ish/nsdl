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
// State panel: rendered from world.state() after every command.
// ------------------------------------------------------------------

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
    const fieldsText = inst.fields.map((f) => f.key + "=" + f.value).join("  ");
    row.innerHTML =
      "<td>" + esc(inst.name) + "</td>" +
      "<td>" + esc(inst.type) + "</td>" +
      "<td>" + esc(inst.state || "—") + "</td>" +
      "<td class=\"fields\">" + esc(fieldsText) + "</td>";
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
