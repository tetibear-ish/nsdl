(function(Object){
   typeof globalThis !== "object"
   &&
    (this
      ? get()
      : (Object.defineProperty
         (Object.prototype, "_T_", {configurable: true, get: get}),
        _T_));
   function get(){
    var global = this || self;
    global.globalThis = global;
    delete Object.prototype._T_;
   }
  }
  (Object));
(js=>
     async args=>{
      "use strict";
      const
       {link, src, generated, disable_effects} = args,
       isNode = globalThis.process?.versions?.node,
       math =
         {cos: Math.cos,
          sin: Math.sin,
          tan: Math.tan,
          acos: Math.acos,
          asin: Math.asin,
          atan: Math.atan,
          cosh: Math.cosh,
          sinh: Math.sinh,
          tanh: Math.tanh,
          acosh: Math.acosh,
          asinh: Math.asinh,
          atanh: Math.atanh,
          cbrt: Math.cbrt,
          exp: Math.exp,
          expm1: Math.expm1,
          log: Math.log,
          log1p: Math.log1p,
          log2: Math.log2,
          log10: Math.log10,
          atan2: Math.atan2,
          hypot: Math.hypot,
          pow: Math.pow,
          fmod: (x, y)=>x % y},
       typed_arrays =
         [Float32Array,
          Float64Array,
          Int8Array,
          Uint8Array,
          Int16Array,
          Uint16Array,
          Int32Array,
          Int32Array,
          Int32Array,
          Int32Array,
          Float32Array,
          Float64Array,
          Uint8Array,
          Uint16Array,
          Uint8ClampedArray],
       fs = isNode && require("node:fs"),
       virtual_files = new Map(),
       virtual_dirs = new Set(),
       virtual_fds = new Map();
      let next_virtual_fd = 1000000;
      function register_virtual_file(name, content){
       virtual_files.set(name, content);
       let dir = name;
       while(true){
        const i = dir.lastIndexOf("/");
        if(i <= 0) break;
        dir = dir.slice(0, i);
        virtual_dirs.add(dir);
       }
      }
      if(args.files)
       for(const [name, data] of Object.entries(args.files))
        register_virtual_file
         (name, Uint8Array.from(atob(data), c=>c.charCodeAt(0)));
      const
       fs_cst = fs?.constants,
       access_flags =
         fs ? [fs_cst.R_OK, fs_cst.W_OK, fs_cst.X_OK, fs_cst.F_OK] : [],
       open_flags =
         fs
          ? [fs_cst.O_RDONLY,
            fs_cst.O_WRONLY,
            fs_cst.O_RDWR,
            fs_cst.O_APPEND,
            fs_cst.O_CREAT,
            fs_cst.O_TRUNC,
            fs_cst.O_EXCL,
            fs_cst.O_NONBLOCK,
            fs_cst.O_NOCTTY,
            fs_cst.O_DSYNC,
            fs_cst.O_SYNC]
          : [];
      var
       out_channels =
         {map: new WeakMap(),
          set: new Set(),
          finalization:
          new FinalizationRegistry(ref=>out_channels.set.delete(ref))};
      function register_channel(ch){
       const ref = new WeakRef(ch);
       out_channels.map.set(ch, ref);
       out_channels.set.add(ref);
       out_channels.finalization.register(ch, ref, ch);
      }
      function unregister_channel(ch){
       const ref = out_channels.map.get(ch);
       if(ref){
        out_channels.map.delete(ch);
        out_channels.set.delete(ref);
        out_channels.finalization.unregister(ch);
       }
      }
      function channel_list(){
       return [...out_channels.set].map(ref=>ref.deref()).filter(ch=>ch);
      }
      var start_fiber;
      function make_suspending(f){
       return WebAssembly?.Suspending ? new WebAssembly.Suspending(f) : f;
      }
      function make_promising(f){
       return ! disable_effects && WebAssembly?.promising && f
               ? WebAssembly.promising(f)
               : f;
      }
      const
       decoder = new TextDecoder("utf-8", {ignoreBOM: 1}),
       encoder = new TextEncoder();
      function hash_int(h, d){
       d = Math.imul(d, 0xcc9e2d51 | 0);
       d = d << 15 | d >>> 17;
       d = Math.imul(d, 0x1b873593);
       h ^= d;
       h = h << 13 | h >>> 19;
       return (h + (h << 2) | 0) + (0xe6546b64 | 0) | 0;
      }
      function jsstring_is_bytes(s){
       for(var i = 0; i < s.length; i++)
        if(s.charCodeAt(i) > 0xff) return false;
       return true;
      }
      function caml_hash_mix_jsbytes(h, s){
       var len = s.length, i, w;
       for(i = 0; i + 4 <= len; i += 4){
        w =
         s.charCodeAt(i) | s.charCodeAt(i + 1) << 8
         | s.charCodeAt(i + 2) << 16
         | s.charCodeAt(i + 3) << 24;
        h = hash_int(h, w);
       }
       w = 0;
       switch(len & 3){
         case 3:
          w = s.charCodeAt(i + 2) << 16;
         case 2:
          w |= s.charCodeAt(i + 1) << 8;
         case 1:
          w |= s.charCodeAt(i); h = hash_int(h, w);
       }
       return h ^ len;
      }
      function hash_string(h, s){
       if(jsstring_is_bytes(s)) return caml_hash_mix_jsbytes(h, s);
       var len = s.length, i, w;
       for(i = 0; i + 2 <= len; i += 2){
        w = s.charCodeAt(i) | s.charCodeAt(i + 1) << 16;
        h = hash_int(h, w);
       }
       if(len & 1) h = hash_int(h, s.charCodeAt(i));
       return h ^ len;
      }
      function getenv(n){
       if(isNode && globalThis.process.env[n] !== undefined)
        return globalThis.process.env[n];
       return globalThis.jsoo_env?.[n];
      }
      let record_backtrace_flag = 0;
      for(const l of getenv("OCAMLRUNPARAM")?.split(",") || []){
       if(l === "b") record_backtrace_flag = 1;
       if(l.startsWith("b=")) record_backtrace_flag = + l.slice(2) ? 1 : 0;
      }
      function alloc_stat(s, large){
       var kind;
       if(s.isFile())
        kind = 0;
       else if(s.isDirectory())
        kind = 1;
       else if(s.isCharacterDevice())
        kind = 2;
       else if(s.isBlockDevice())
        kind = 3;
       else if(s.isSymbolicLink())
        kind = 4;
       else if(s.isFIFO()) kind = 5; else if(s.isSocket()) kind = 6;
       return caml_alloc_stat
               (large,
                s.dev,
                s.ino | 0,
                kind,
                s.mode & 0o7777,
                s.nlink,
                s.uid,
                s.gid,
                s.rdev,
                BigInt(s.size),
                s.atimeMs / 1000,
                s.mtimeMs / 1000,
                s.ctimeMs / 1000);
      }
      const
       on_windows = isNode && globalThis.process.platform === "win32",
       on_arm64 = globalThis.process?.arch === "arm64",
       isV8 = new Error().stack?.includes("\n    at ") ?? false,
       call = Function.prototype.call,
       DV = DataView.prototype,
       bindings =
         {jstag:
          WebAssembly.JSTag
          || new WebAssembly.Tag({parameters: ["externref"], results: []}),
          identity: x=>x,
          from_bool: x=>! ! x,
          get: (x, y)=>x[y],
          set: (x, y, z)=>x[y] = z,
          delete: (x, y)=>delete x[y],
          instanceof: (x, y)=>x instanceof y,
          is_js_error: x=>x instanceof Error,
          to_js_string: x=>String(x),
          typeof: x=>typeof x,
          equals: (x, y)=>x == y,
          strict_equals: (x, y)=>x === y,
          fun_call: (f, o, args)=>f.apply(o, args),
          meth_call: (o, f, args)=>o[f].apply(o, args),
          new_array: n=>new Array(n),
          new_obj: ()=>({}),
          new: (c, args)=>new c(...args),
          global_this: globalThis,
          iter_props:
          (o, f)=>{for(var nm in o) if(Object.hasOwn(o, nm)) f(nm);},
          array_length: a=>a.length,
          array_get: (a, i)=>a[i],
          array_set: (a, i, v)=>a[i] = v,
          read_string: l=>decoder.decode(new Uint8Array(buffer, 0, l)),
          read_string_stream:
          (l, stream)=>
             decoder.decode(new Uint8Array(buffer, 0, l), {stream: stream}),
          append_string: (s1, s2)=>s1 + s2,
          write_string:
          s=>{
           var start = 0, len = s.length;
           for(;;){
            const
             {read, written} = encoder.encodeInto(s.slice(start), out_buffer);
            len -= read;
            if(! len) return written;
            caml_extract_bytes(written);
            start += read;
           }},
          ta_create: (k, sz)=>new typed_arrays[k](sz),
          ta_normalize:
          a=>
             a instanceof Uint32Array
              ? new Int32Array(a.buffer, a.byteOffset, a.length)
              : a,
          ta_kind: a=>typed_arrays.findIndex(c=>a instanceof c),
          ta_length: a=>a.length,
          ta_get_i32: (a, i)=>a[i],
          ta_fill: (a, v)=>a.fill(v),
          ta_blit: (s, d)=>d.set(s),
          ta_subarray: (a, i, j)=>a.subarray(i, j),
          ta_set: (a, b, i)=>a.set(b, i),
          ta_new: len=>new Uint8Array(len),
          ta_copy: (ta, t, s, e)=>ta.copyWithin(t, s, e),
          ta_bytes:
          a=>
             new
              Uint8Array
              (a.buffer, a.byteOffset, a.length * a.BYTES_PER_ELEMENT),
          dv_make: a=>new DataView(a.buffer, a.byteOffset, a.byteLength),
          dv_get_f64: call.bind(DV.getFloat64),
          dv_get_f32: call.bind(DV.getFloat32),
          dv_get_i64: call.bind(DV.getBigInt64),
          dv_get_i32:
          isV8 ? call.bind(DV.getInt32) : (x, y, z)=>x.getInt32(y, z),
          dv_get_i16: call.bind(DV.getInt16),
          dv_get_ui16: call.bind(DV.getUint16),
          dv_get_i8: call.bind(DV.getInt8),
          dv_get_ui8: call.bind(DV.getUint8),
          dv_set_f64: call.bind(DV.setFloat64),
          dv_set_f32: call.bind(DV.setFloat32),
          dv_set_i64: call.bind(DV.setBigInt64),
          dv_set_i32: call.bind(DV.setInt32),
          dv_set_i16: call.bind(DV.setInt16),
          dv_set_i8: call.bind(DV.setInt8),
          littleEndian: new Uint8Array(new Uint32Array([1]).buffer)[0],
          wrap_callback:
          f=>
             function(...args){
              if(args.length === 0) args = [undefined];
              return caml_callback(f, args.length, args, 1);
             },
          wrap_callback_args:
          f=>function(...args){return caml_callback(f, 1, [args], 0);},
          wrap_callback_strict:
          (arity, f)=>
             function(...args){
              args.length = arity;
              return caml_callback(f, arity, args, 0);
             },
          wrap_callback_unsafe:
          f=>function(...args){return caml_callback(f, args.length, args, 2);},
          wrap_meth_callback:
          f=>
             function(...args){
              args.unshift(this);
              return caml_callback(f, args.length, args, 1);
             },
          wrap_meth_callback_args:
          f=>function(...args){return caml_callback(f, 2, [this, args], 0);},
          wrap_meth_callback_strict:
          (arity, f)=>
             function(...args){
              args.length = arity;
              args.unshift(this);
              return caml_callback(f, args.length, args, 0);
             },
          wrap_meth_callback_unsafe:
          f=>
             function(...args){
              args.unshift(this);
              return caml_callback(f, args.length, args, 2);
             },
          wrap_fun_arguments: f=>function(...args){return f(args);},
          format_float:
          (prec, conversion, pad, x)=>{
           function decompose(x){
            var dv = new DataView(new ArrayBuffer(8));
            dv.setFloat64(0, x);
            var
             hi = dv.getUint32(0),
             lo = dv.getUint32(4),
             eb = hi >>> 20 & 0x7ff,
             m = BigInt(hi & 0xfffff) << 32n | BigInt(lo);
            if(eb === 0) return [m, - 1074];
            return [m | 1n << 52n, eb - 1075];
           }
           function exact_scaled(x, k){
            var d = decompose(x), num = d[0], den = 1n;
            if(k >= 0) num *= 10n ** BigInt(k); else den = 10n ** BigInt(- k);
            if(d[1] >= 0) num <<= BigInt(d[1]); else den <<= BigInt(- d[1]);
            var q = num / den, r2 = num % den * 2n;
            if(r2 > den || r2 === den && q & 1n) q += 1n;
            return q;
           }
           function exact_fixed(x, prec){
            var q = exact_scaled(x, prec).toString();
            if(prec === 0) return q;
            if(q.length <= prec) q = "0".repeat(prec + 1 - q.length) + q;
            return q.slice(0, q.length - prec) + "."
                   + q.slice(q.length - prec);
           }
           function exact_exponential(x, prec){
            if(x === 0)
             return (prec > 0 ? "0." + "0".repeat(prec) : "0") + "e+0";
            var e10 = Math.floor(Math.log10(x));
            for(;;){
             var s = exact_scaled(x, prec - e10).toString();
             if(s.length === prec + 1){
              var m = prec > 0 ? s.charAt(0) + "." + s.slice(1) : s;
              return m + "e" + (e10 < 0 ? "-" : "+") + Math.abs(e10);
             }
             e10 += s.length - (prec + 1);
            }
           }
           function toExponential(x, prec){
            return prec > 100
                    ? exact_exponential(x, prec)
                    : x.toExponential(prec);
           }
           function toFixed(x, dp){
            if(dp > 100 || x >= 1e21) return exact_fixed(x, dp);
            return x.toFixed(dp);
           }
           switch(conversion){
             case 0:
              var s = toExponential(x, prec), i = s.length;
              if(s.charAt(i - 3) === "e")
               s = s.slice(0, i - 1) + "0" + s.slice(i - 1);
              break;
             case 1:
              s = toFixed(x, prec); break;
             case 2:
              prec = prec ? prec : 1;
              s = toExponential(x, prec - 1);
              var j = s.indexOf("e"), exp = + s.slice(j + 1);
              if(exp < - 4 || x >= 1e21 || x.toFixed(0).length > prec){
               var i = j - 1;
               while(s.charAt(i) === "0") i--;
               if(s.charAt(i) === ".") i--;
               s = s.slice(0, i + 1) + s.slice(j);
               i = s.length;
               if(s.charAt(i - 3) === "e")
                s = s.slice(0, i - 1) + "0" + s.slice(i - 1);
               break;
              }
              else{
               var p = prec;
               if(exp < 0){
                p -= exp + 1;
                s = toFixed(x, p);
               }
               else
                while(s = toFixed(x, p), s.length > prec + 1) p--;
               if(p){
                var i = s.length - 1;
                while(s.charAt(i) === "0") i--;
                if(s.charAt(i) === ".") i--;
                s = s.slice(0, i + 1);
               }
              }
              break;
           }
           return pad ? " " + s : s;},
          gettimeofday: ()=>Date.now() / 1000,
          times:
          ()=>{
           if(globalThis.process?.cpuUsage){
            var t = globalThis.process.cpuUsage();
            return caml_alloc_times(t.user / 1e6, t.system / 1e6);
           }
           else{
            var t = performance.now() / 1000;
            return caml_alloc_times(t, 0);
           }},
          gmtime:
          t=>{
           var
            d = new Date(t * 1000),
            d_num = d.getTime(),
            januaryfirst =
              new Date(Date.UTC(d.getUTCFullYear(), 0, 1)).getTime(),
            doy = Math.floor((d_num - januaryfirst) / 86400000);
           return caml_alloc_tm
                   (d.getUTCSeconds(),
                    d.getUTCMinutes(),
                    d.getUTCHours(),
                    d.getUTCDate(),
                    d.getUTCMonth(),
                    d.getUTCFullYear() - 1900,
                    d.getUTCDay(),
                    doy,
                    false);},
          localtime:
          t=>{
           var
            d = new Date(t * 1000),
            doy =
              Math.floor
               ((Date.UTC(d.getFullYear(), d.getMonth(), d.getDate())
                - Date.UTC(d.getFullYear(), 0, 1))
                / 86400000),
            jan = new Date(d.getFullYear(), 0, 1),
            jul = new Date(d.getFullYear(), 6, 1),
            stdTimezoneOffset =
              Math.max(jan.getTimezoneOffset(), jul.getTimezoneOffset()),
            isdst = d.getTimezoneOffset() < stdTimezoneOffset;
           if
            (stdTimezoneOffset === 0
             && jan.getTimezoneOffset() !== jul.getTimezoneOffset()
             &&
              globalThis.Intl?.DateTimeFormat?.().resolvedOptions().timeZone
              === "Europe/Dublin")
            isdst = ! isdst;
           return caml_alloc_tm
                   (d.getSeconds(),
                    d.getMinutes(),
                    d.getHours(),
                    d.getDate(),
                    d.getMonth(),
                    d.getFullYear() - 1900,
                    d.getDay(),
                    doy,
                    isdst);},
          mktime:
          (year, month, day, h, m, s)=>
             new Date(year, month, day, h, m, s).getTime(),
          random_seed: ()=>crypto.getRandomValues(new Int32Array(12)),
          access:
          (p, flags)=>
             fs.accessSync
              (p,
               access_flags.reduce((f, v, i)=>flags & 1 << i ? f | v : f, 0)),
          open:
          (p, flags, perm)=>{
           if(virtual_files.has(p) && ! (flags & 2)){
            const fd = next_virtual_fd++;
            virtual_fds.set(fd, {data: virtual_files.get(p), offset: 0});
            return fd;
           }
           return fs.openSync
                   (p,
                    open_flags.reduce((f, v, i)=>flags & 1 << i ? f | v : f, 0),
                    perm);},
          close:
          fd=>{
           if(virtual_fds.has(fd)){virtual_fds.delete(fd); return;}
           fs.closeSync(fd);},
          write:
          (fd, b, o, l, p)=>
             fs
              ? fs.writeSync(fd, b, o, l, p === null ? p : Number(p))
              : (console
                  [fd === 2 ? "error" : "log"]
                 (typeof b === "string"
                   ? b
                   : decoder.decode(b.slice(o, o + l))),
                l),
          read:
          (fd, b, o, l, p)=>{
           const vf = virtual_fds.get(fd);
           if(vf){
            const
             pos = p === null ? vf.offset : Number(p),
             n = Math.min(l, vf.data.length - pos);
            if(n <= 0) return 0;
            b.set(vf.data.subarray(pos, pos + n), o);
            vf.offset = pos + n;
            return n;
           }
           return fs.readSync(fd, b, o, l, p);},
          fsync: fd=>fs.fsyncSync(fd),
          file_size:
          fd=>{
           const vf = virtual_fds.get(fd);
           if(vf) return BigInt(vf.data.length);
           return fs.fstatSync(fd, {bigint: true}).size;},
          register_channel: register_channel,
          unregister_channel: unregister_channel,
          channel_list: channel_list,
          exit: n=>isNode && globalThis.process.exit(n),
          argv: ()=>isNode ? globalThis.process.argv.slice(1) : ["a.out"],
          on_windows: + on_windows,
          on_arm64: + on_arm64,
          getenv: getenv,
          backtrace_status: ()=>record_backtrace_flag,
          record_backtrace: b=>record_backtrace_flag = b,
          system:
          c=>{
           var
            res =
              require("node:child_process").spawnSync
               (c, {shell: true, stdio: "inherit"});
           if(res.error) throw res.error;
           return res.signal ? 255 : res.status;},
          isatty: fd=>isNode ? + require("node:tty").isatty(fd) : 0,
          getuid:
          ()=>globalThis.process?.getuid ? globalThis.process.getuid() : 1,
          geteuid:
          ()=>globalThis.process?.geteuid ? globalThis.process.geteuid() : 1,
          getgid:
          ()=>globalThis.process?.getgid ? globalThis.process.getgid() : 1,
          getegid:
          ()=>globalThis.process?.getegid ? globalThis.process.getegid() : 1,
          time: ()=>performance.now(),
          getcwd: ()=>isNode ? globalThis.process.cwd() : "/static",
          chdir: x=>globalThis.process.chdir(x),
          mkdir: (p, m)=>fs.mkdirSync(p, m),
          rmdir: p=>fs.rmdirSync(p),
          link: (d, s)=>fs.linkSync(d, s),
          symlink:
          (t, p, kind)=>fs.symlinkSync(t, p, [null, "file", "dir"][kind]),
          readlink: p=>fs.readlinkSync(p),
          unlink: p=>fs.unlinkSync(p),
          read_dir:
          p=>{
           const prefix = p.endsWith("/") ? p : p + "/", entries = new Set();
           for(const name of virtual_files.keys())
            if(name.startsWith(prefix)){
             const
              rest = name.slice(prefix.length),
              slash = rest.indexOf("/");
             entries.add(slash < 0 ? rest : rest.slice(0, slash));
            }
           if(fs)
            try{for(const e of fs.readdirSync(p)) entries.add(e);}
            catch(e){if(entries.size === 0) throw e;}
           return [...entries];},
          opendir: p=>({dir: fs.opendirSync(p), dots: [".", ".."]}),
          readdir:
          d=>{
           if(d.dots.length > 0) return d.dots.shift();
           var n = d.dir.readSync()?.name;
           return n === undefined ? null : n;},
          closedir: d=>{d.dots = []; d.dir.closeSync();},
          stat: (p, l)=>alloc_stat(fs.statSync(p), l),
          lstat: (p, l)=>alloc_stat(fs.lstatSync(p), l),
          fstat: (fd, l)=>alloc_stat(fs.fstatSync(fd), l),
          chmod: (p, perms)=>fs.chmodSync(p, perms),
          fchmod: (p, perms)=>fs.fchmodSync(p, perms),
          file_exists:
          p=>{
           if(virtual_files.has(p) || virtual_dirs.has(p)) return 1;
           return fs ? + fs.existsSync(p) : 0;},
          is_directory:
          p=>{
           if(virtual_dirs.has(p)) return 1;
           if(virtual_files.has(p)) return 0;
           return + fs.statSync(p).isDirectory();},
          is_file:
          p=>{
           if(virtual_files.has(p)) return 1;
           if(virtual_dirs.has(p)) return 0;
           return + fs.statSync(p).isFile();},
          utimes: (p, a, m)=>fs.utimesSync(p, a, m),
          truncate: (p, l)=>fs.truncateSync(p, l),
          ftruncate: (fd, l)=>fs.ftruncateSync(fd, l),
          rename:
          (o, n)=>{
           var n_stat;
           if
            (on_windows && (n_stat = fs.statSync(n, {throwIfNoEntry: false}))
             && fs.statSync(o, {throwIfNoEntry: false})?.isDirectory())
            if(n_stat.isDirectory()){
             if(! n.startsWith(o)) try{fs.rmdirSync(n);}catch{}
            }
            else{
             var
              e =
                new Error(`ENOTDIR: not a directory, rename '${o}' -> '${n}'`);
             throw Object.assign
                    (e,
                     {errno: - 20, code: "ENOTDIR", syscall: "rename", path: n});
            }
           fs.renameSync(o, n);},
          tmpdir: ()=>require("node:os").tmpdir(),
          start_fiber: x=>start_fiber(x),
          suspend_fiber: make_suspending((f, env)=>new Promise(k=>f(k, env))),
          resume_fiber: (k, v)=>k(v),
          weak_new: v=>new WeakRef(v),
          weak_deref:
          w=>{var v = w.deref(); return v === undefined ? null : v;},
          weak_map_new: ()=>new WeakMap(),
          map_new: ()=>new Map(),
          map_get:
          (m, x)=>{var v = m.get(x); return v === undefined ? null : v;},
          map_set: (m, x, v)=>m.set(x, v),
          map_delete: (m, x)=>m.delete(x),
          hash_string: hash_string,
          log: x=>console.log(x),
          register_fragments:
          (unitName, fragmentsSource)=>{
           const frags = eval?.(fragmentsSource);
           imports[unitName + ".fragments"] = frags;},
          load_module:
          wasmBytes=>{
           const
            module = new WebAssembly.Module(wasmBytes, options),
            inst = new WebAssembly.Instance(module, imports);
           Object.assign(imports.OCaml, inst.exports);
           return inst.exports["_dynlink.init"]();},
          load_wasmo:
          zipBytes=>{
           const
            dv =
              new
               DataView
               (zipBytes.buffer, zipBytes.byteOffset, zipBytes.byteLength),
            len = zipBytes.byteLength;
           let eocdOff = len - 22;
           while(eocdOff >= 0 && dv.getUint32(eocdOff, true) !== 0x06054b50)
            eocdOff--;
           if(eocdOff < 0) throw new Error("Invalid ZIP: EOCD not found");
           const
            cdOff = dv.getUint32(eocdOff + 16, true),
            cdEntries = dv.getUint16(eocdOff + 10, true),
            entries = {};
           let off = cdOff;
           for(let i = 0; i < cdEntries; i++){
            if(dv.getUint32(off, true) !== 0x02014b50)
             throw new Error("Invalid ZIP: bad CD entry");
            const
             nameLen = dv.getUint16(off + 28, true),
             extraLen = dv.getUint16(off + 30, true),
             commentLen = dv.getUint16(off + 32, true),
             localOff = dv.getUint32(off + 42, true),
             name =
               decoder.decode(zipBytes.subarray(off + 46, off + 46 + nameLen)),
             size = dv.getUint32(off + 24, true),
             localNameLen = dv.getUint16(localOff + 26, true),
             localExtraLen = dv.getUint16(localOff + 28, true),
             dataOff = localOff + 30 + localNameLen + localExtraLen;
            entries[name] = zipBytes.subarray(dataOff, dataOff + size);
            off += 46 + nameLen + extraLen + commentLen;
           }
           if(! entries["code.wasm"])
            throw new Error("code.wasm not found in .wasmo");
           const
            module = new WebAssembly.Module(entries["code.wasm"], options),
            inst = new WebAssembly.Instance(module, imports);
           Object.assign(imports.OCaml, inst.exports);
           const names = decoder.decode(entries.link_order).split("\x00");
           for(const name of names) inst.exports[name + ".init"]();},
          register_file: (name, data)=>register_virtual_file(name, data),
          read_file: name=>virtual_files.get(name) ?? null},
       string_ops =
         {test: v=>+ (typeof v === "string"),
          compare: (s1, s2)=>s1 < s2 ? - 1 : + (s1 > s2),
          decodeStringFromUTF8Array: ()=>"",
          encodeStringToUTF8Array: ()=>0,
          fromCharCodeArray: ()=>"",
          length: s=>s.length,
          intoCharCodeArray: ()=>0},
       imports =
         Object.assign
          ({Math: math,
            bindings: bindings,
            js: js,
            "wasm:js-string": string_ops,
            "wasm:text-decoder": string_ops,
            "wasm:text-encoder": string_ops,
            str: new globalThis.Proxy({}, {get(_, prop){return prop;}}),
            env: {}},
           generated),
       options =
         {builtins: ["js-string", "text-decoder", "text-encoder"],
          importedStringConstants: "str"};
      function loadRelative(src){
       const
        path = require("node:path"),
        f = path.join(path.dirname(require.main.filename), src);
       return require("node:fs/promises").readFile(f);
      }
      const fetchBase = globalThis?.document?.currentScript?.src;
      function fetchRelative(src){
       const url = fetchBase ? new URL(src, fetchBase) : src;
       return fetch(url);
      }
      const loadCode = isNode ? loadRelative : fetchRelative;
      async function instantiateModule(code){
       return isNode
               ? WebAssembly.instantiate(await code, imports, options)
               : WebAssembly.instantiateStreaming(code, imports, options);
      }
      async function instantiateFromDir(){
       imports.OCaml = {};
       const deps = [];
       async function loadModule(module, isRuntime){
        const sync = module[1].constructor !== Array;
        async function instantiate(){
         const code = loadCode(src + "/" + module[0] + ".wasm");
         await Promise.all(sync ? deps : module[1].map(i=>deps[i]));
         const wasmModule = await instantiateModule(code);
         Object.assign
          (isRuntime ? imports.env : imports.OCaml,
           wasmModule.instance.exports);
        }
        const promise = instantiate();
        deps.push(promise);
        return promise;
       }
       async function loadModules(lst){
        for(const module of lst) await loadModule(module);
       }
       await loadModule(link[0], 1);
       if(link.length > 1){
        await loadModule(link[1]);
        const
         workers = new Array(20).fill(link.slice(2).values()).map(loadModules);
        await Promise.all(workers);
       }
       return {instance: {exports: Object.assign(imports.env, imports.OCaml)}};
      }
      const wasmModule = await instantiateFromDir();
      var
       {caml_callback,
         caml_alloc_times,
         caml_alloc_tm,
         caml_alloc_stat,
         caml_start_fiber,
         caml_handle_uncaught_exception,
         caml_buffer,
         caml_extract_bytes,
         _initialize}
        = wasmModule.instance.exports,
       buffer = caml_buffer?.buffer,
       out_buffer = buffer && new Uint8Array(buffer, 0, buffer.length);
      start_fiber = make_promising(caml_start_fiber);
      var _initialize = make_promising(_initialize);
      if(globalThis.process?.on)
       globalThis.process.on
        ("uncaughtException",
         (err, _origin)=>caml_handle_uncaught_exception(err));
      else if(globalThis.addEventListener)
       globalThis.addEventListener
        ("error",
         event=>event.error && caml_handle_uncaught_exception(event.error));
      await _initialize();})
 (function(globalThis){
    "use strict";
    function caml_js_html_entities(s){
     var entity = /^&#?[0-9a-zA-Z]+;$/;
     if(s.match(entity)){
      var str, temp = document.createElement("p");
      temp.innerHTML = s;
      str = temp.textContent || temp.innerText;
      temp = null;
      return str;
     }
     else
      return null;
    }
    function caml_jsoo_promise_wrapper(x){this.wrapped = x;}
    function caml_jsoo_promise_unwrap(x){
     return x instanceof caml_jsoo_promise_wrapper ? x.wrapped : x;
    }
    function caml_jsoo_promise_wrap(x){
     return x != null && typeof x.then === "function"
             ? new caml_jsoo_promise_wrapper(x)
             : x;
    }
    var
     unix_error =
       ["E2BIG",
        "EACCES",
        "EAGAIN",
        "EBADF",
        "EBUSY",
        "ECHILD",
        "EDEADLK",
        "EDOM",
        "EEXIST",
        "EFAULT",
        "EFBIG",
        "EINTR",
        "EINVAL",
        "EIO",
        "EISDIR",
        "EMFILE",
        "EMLINK",
        "ENAMETOOLONG",
        "ENFILE",
        "ENODEV",
        "ENOENT",
        "ENOEXEC",
        "ENOLCK",
        "ENOMEM",
        "ENOSPC",
        "ENOSYS",
        "ENOTDIR",
        "ENOTEMPTY",
        "ENOTTY",
        "ENXIO",
        "EPERM",
        "EPIPE",
        "ERANGE",
        "EROFS",
        "ESPIPE",
        "ESRCH",
        "EXDEV",
        "EWOULDBLOCK",
        "EINPROGRESS",
        "EALREADY",
        "ENOTSOCK",
        "EDESTADDRREQ",
        "EMSGSIZE",
        "EPROTOTYPE",
        "ENOPROTOOPT",
        "EPROTONOSUPPORT",
        "ESOCKTNOSUPPORT",
        "EOPNOTSUPP",
        "EPFNOSUPPORT",
        "EAFNOSUPPORT",
        "EADDRINUSE",
        "EADDRNOTAVAIL",
        "ENETDOWN",
        "ENETUNREACH",
        "ENETRESET",
        "ECONNABORTED",
        "ECONNRESET",
        "ENOBUFS",
        "EISCONN",
        "ENOTCONN",
        "ESHUTDOWN",
        "ETOOMANYREFS",
        "ETIMEDOUT",
        "ECONNREFUSED",
        "EHOSTDOWN",
        "EHOSTUNREACH",
        "ELOOP",
        "EOVERFLOW"];
    function caml_strerror(errno){
     if(typeof require === "undefined"){
      const code = unix_error[errno];
      return code || "Unknown error " + errno;
     }
     const util = require("node:util");
     if(errno >= 0){
      const code = unix_error[errno];
      for(const e of util.getSystemErrorMap())
       if(e[1][0] === code) return e[1][1];
      return code || "Unknown error " + errno;
     }
     else
      return util.getSystemErrorMessage(errno);
    }
    var caml_gr_state;
    function caml_gr_y(s, y){return s.height - 1 - y;}
    function gr_blit_image_for_wasm(im, x, y){
     var
      s = caml_gr_state,
      im2 =
        s.context.getImageData
         (x, caml_gr_y(s, y) - im.height + 1, im.width, im.height);
     for(var i = 0; i < im2.data.length; i += 4){
      im.data[i] = im2.data[i];
      im.data[i + 1] = im2.data[i + 1];
      im.data[i + 2] = im2.data[i + 2];
      im.data[i + 3] = im2.data[i + 3];
     }
    }
    function gr_clear_for_wasm(){
     var s = caml_gr_state;
     s.context.clearRect(0, 0, s.canvas.width, s.canvas.height);
    }
    function gr_close_for_wasm(){
     caml_gr_state.canvas.width = 0;
     caml_gr_state.canvas.height = 0;
    }
    function gr_create_image_for_wasm(x, y){
     return caml_gr_state.context.createImageData(x, y);
    }
    function gr_current_x_for_wasm(){return caml_gr_state.x;}
    function gr_current_y_for_wasm(){return caml_gr_state.y;}
    function gr_doc_of_state_for_wasm(state){
     if(state.canvas.ownerDocument) return state.canvas.ownerDocument;
     return null;
    }
    function caml_gr_arc_aux(ctx, cx, cy, ry, rx, a1, a2){
     while(a1 > a2) a2 += 360;
     a1 /= 180;
     a2 /= 180;
     var
      rot = 0,
      xPos,
      yPos,
      xPos_prev,
      yPos_prev,
      space = 2,
      num = (a2 - a1) * Math.PI * ((rx + ry) / 2) / space | 0,
      delta = (a2 - a1) * Math.PI / num,
      i = - a1 * Math.PI;
     for(var j = 0; j <= num; j++){
      xPos =
       cx - rx * Math.sin(i) * Math.sin(rot * Math.PI)
       + ry * Math.cos(i) * Math.cos(rot * Math.PI);
      xPos = xPos.toFixed(2);
      yPos =
       cy + ry * Math.cos(i) * Math.sin(rot * Math.PI)
       + rx * Math.sin(i) * Math.cos(rot * Math.PI);
      yPos = yPos.toFixed(2);
      if(j === 0)
       ctx.moveTo(xPos, yPos);
      else if(xPos_prev !== xPos || yPos_prev !== yPos)
       ctx.lineTo(xPos, yPos);
      xPos_prev = xPos;
      yPos_prev = yPos;
      i -= delta;
     }
     return 0;
    }
    function caml_gr_xc(x){return x + 0.5;}
    function caml_gr_yc(s, y){return caml_gr_y(s, y) + 0.5;}
    function gr_draw_arc_for_wasm(x, y, rx, ry, a1, a2){
     var s = caml_gr_state;
     s.context.beginPath();
     caml_gr_arc_aux
      (s.context, caml_gr_xc(x), caml_gr_yc(s, y), rx, ry, a1, a2);
     s.context.stroke();
    }
    function gr_draw_str_for_wasm(str){
     var s = caml_gr_state, m = s.context.measureText(str), dx = m.width;
     s.context.fillText(str, s.x, caml_gr_y(s, s.y) + 1);
     s.x += dx | 0;
    }
    function gr_draw_char_for_wasm(c){
     gr_draw_str_for_wasm(String.fromCharCode(c));
    }
    function gr_draw_image_for_wasm(im, x, y){
     var s = caml_gr_state;
     if(! im.image){
      var canvas = document.createElement("canvas");
      canvas.width = s.width;
      canvas.height = s.height;
      canvas.getContext("2d").putImageData(im, 0, 0);
      im.image = canvas;
     }
     s.context.drawImage(im.image, x, caml_gr_y(s, y) - im.height + 1);
    }
    function gr_draw_rect_for_wasm(x, y, w, h){
     var
      s = caml_gr_state,
      x0 = caml_gr_xc(x),
      y0 = caml_gr_yc(s, y),
      x1 = caml_gr_xc(x + w),
      y1 = caml_gr_yc(s, y + h);
     s.context.beginPath();
     s.context.moveTo(x0, y0);
     s.context.lineTo(x1, y0);
     s.context.lineTo(x1, y1);
     s.context.lineTo(x0, y1);
     s.context.lineTo(x0, y0);
     s.context.stroke();
    }
    function gr_dump_image_height_for_wasm(im){return im.height;}
    function gr_dump_image_pixel_for_wasm(im, i, j){
     var o = i * (im.width * 4) + j * 4;
     return (im.data[o] << 16) + (im.data[o + 1] << 8) + im.data[o + 2];
    }
    function gr_dump_image_width_for_wasm(im){return im.width;}
    function gr_fill_arc_for_wasm(x, y, rx, ry, a1, a2){
     var s = caml_gr_state, cx = caml_gr_xc(x), cy = caml_gr_yc(s, y);
     s.context.beginPath();
     caml_gr_arc_aux(s.context, cx, cy, rx, ry, a1, a2);
     s.context.lineTo(cx, cy);
     s.context.fill();
    }
    function gr_fill_poly_for_wasm(ar, n){
     var s = caml_gr_state;
     s.context.beginPath();
     s.context.moveTo(ar[0], caml_gr_y(s, ar[1]));
     for(var i = 1; i < n; i++)
      s.context.lineTo(ar[i * 2], caml_gr_y(s, ar[i * 2 + 1]));
     s.context.lineTo(ar[0], caml_gr_y(s, ar[1]));
     s.context.fill();
    }
    function gr_fill_rect_for_wasm(x, y, w, h){
     var s = caml_gr_state;
     s.context.fillRect(x, caml_gr_y(s, y) - h, w + 1, h + 1);
    }
    function gr_lineto_for_wasm(x, y){
     var s = caml_gr_state;
     s.context.beginPath();
     s.context.moveTo(caml_gr_xc(s.x), caml_gr_yc(s, s.y));
     s.context.lineTo(caml_gr_xc(x), caml_gr_yc(s, y));
     s.context.stroke();
     s.x = x;
     s.y = y;
    }
    function gr_make_image_for_wasm(pixels, w, h){
     var s = caml_gr_state, im = s.context.createImageData(w, h);
     for(var i = 0; i < h; i++)
      for(var j = 0; j < w; j++){
       var c = pixels[i * w + j], o = i * (w * 4) + j * 4;
       if(c === - 1){
        im.data[o + 0] = 0;
        im.data[o + 1] = 0;
        im.data[o + 2] = 0;
        im.data[o + 3] = 0;
       }
       else{
        im.data[o + 0] = c >> 16 & 0xff;
        im.data[o + 1] = c >> 8 & 0xff;
        im.data[o + 2] = c >> 0 & 0xff;
        im.data[o + 3] = 0xff;
       }
      }
     return im;
    }
    function gr_moveto_for_wasm(x, y){
     caml_gr_state.x = x;
     caml_gr_state.y = y;
    }
    function gr_state_create_for_wasm(canvas, w, h){
     var context = canvas.getContext("2d");
     context.lineCap = "round";
     context.lineJoin = "round";
     return {context: context,
             canvas: canvas,
             x: 0,
             y: 0,
             width: w,
             height: h,
             line_width: 1,
             font: "fixed",
             text_size: 26,
             color: 0x000000,
             title: ""};
    }
    function gr_set_font_for_wasm(f){
     var s = caml_gr_state;
     s.font = f;
     s.context.font = s.text_size + "px " + f;
    }
    function gr_set_window_title_for_wasm(name){
     var s = caml_gr_state;
     s.title = name;
     if(s.set_title) s.set_title(name);
    }
    function gr_set_line_width_for_wasm(w){
     caml_gr_state.line_width = w;
     caml_gr_state.context.lineWidth = w;
     caml_gr_state.context.lineCap = "round";
     caml_gr_state.context.lineJoin = "round";
    }
    function gr_set_text_size_for_wasm(size){
     var s = caml_gr_state;
     s.text_size = size;
     s.context.font = s.text_size + "px " + s.font;
    }
    function gr_set_color_for_wasm(color){
     var s = caml_gr_state;
     function convert(number){
      var str = "" + number.toString(16);
      while(str.length < 2) str = "0" + str;
      return str;
     }
     var r = color >> 16 & 0xff, g = color >> 8 & 0xff, b = color >> 0 & 0xff;
     s.color = color;
     var c_str = "#" + convert(r) + convert(g) + convert(b);
     s.context.fillStyle = c_str;
     s.context.strokeStyle = c_str;
    }
    function gr_resize_window_for_wasm(w, h){
     var s = caml_gr_state;
     s.width = w;
     s.height = h;
     if(w !== s.canvas.width) s.canvas.width = w;
     if(h !== s.canvas.height) s.canvas.height = h;
    }
    function gr_state_init_for_wasm(){
     var s = caml_gr_state;
     gr_moveto_for_wasm(s.x, s.y);
     gr_resize_window_for_wasm(s.width, s.height);
     gr_set_line_width_for_wasm(s.line_width);
     gr_set_text_size_for_wasm(s.text_size);
     gr_set_font_for_wasm(s.font);
     gr_set_color_for_wasm(s.color);
     gr_set_window_title_for_wasm(s.title);
     s.context.textBaseline = "bottom";
    }
    function gr_state_set_for_wasm(ctx){
     caml_gr_state = ctx;
     gr_state_init_for_wasm();
    }
    function gr_open_for_wasm(info){
     function get(name){
      var res = info.match("(^|,) *" + name + " *= *([a-zA-Z0-9_]+) *(,|$)");
      if(res) return res[2];
     }
     var specs = [];
     if(! (info === "")) specs.push(info);
     var target = get("target");
     if(! target) target = "";
     var status = get("status");
     if(! status) specs.push("status=1");
     var w = get("width");
     w = w ? Number.parseInt(w) : 200;
     specs.push("width=" + w);
     var h = get("height");
     h = h ? Number.parseInt(h) : 200;
     specs.push("height=" + h);
     var win = globalThis.open("about:blank", target, specs.join(","));
     if(! win) return - 1;
     var doc = win.document, canvas = doc.createElement("canvas");
     canvas.width = w;
     canvas.height = h;
     var ctx = gr_state_create_for_wasm(canvas, w, h);
     ctx.set_title = function(title){doc.title = title;};
     gr_state_set_for_wasm(ctx);
     var body = doc.body;
     body.style.margin = "0px";
     body.appendChild(canvas);
     return 0;
    }
    function gr_plot_for_wasm(x, y){
     var
      s = caml_gr_state,
      im = s.context.createImageData(1, 1),
      d = im.data,
      color = s.color;
     d[0] = color >> 16 & 0xff;
     d[1] = color >> 8 & 0xff;
     d[2] = color >> 0 & 0xff;
     d[3] = 0xff;
     s.x = x;
     s.y = y;
     s.context.putImageData(im, x, caml_gr_y(s, y));
    }
    function gr_point_color_for_wasm(x, y){
     var
      s = caml_gr_state,
      im = s.context.getImageData(x, caml_gr_y(s, y), 1, 1),
      d = im.data;
     return (d[0] << 16) + (d[1] << 8) + d[2];
    }
    function gr_size_x_for_wasm(){return caml_gr_state.width;}
    function gr_size_y_for_wasm(){return caml_gr_state.height;}
    function gr_state_for_wasm(){
     if(caml_gr_state) return caml_gr_state;
     return null;
    }
    function gr_text_size_h_for_wasm(){return caml_gr_state.text_size;}
    function gr_text_size_w_for_wasm(txt){
     return caml_gr_state.context.measureText(txt).width | 0;
    }
    return {unix_error: unix_error,
            gr_text_size_w_for_wasm: gr_text_size_w_for_wasm,
            gr_text_size_h_for_wasm: gr_text_size_h_for_wasm,
            gr_state_set_for_wasm: gr_state_set_for_wasm,
            gr_state_for_wasm: gr_state_for_wasm,
            gr_state_create_for_wasm: gr_state_create_for_wasm,
            gr_size_y_for_wasm: gr_size_y_for_wasm,
            gr_size_x_for_wasm: gr_size_x_for_wasm,
            gr_set_window_title_for_wasm: gr_set_window_title_for_wasm,
            gr_set_text_size_for_wasm: gr_set_text_size_for_wasm,
            gr_set_line_width_for_wasm: gr_set_line_width_for_wasm,
            gr_set_font_for_wasm: gr_set_font_for_wasm,
            gr_set_color_for_wasm: gr_set_color_for_wasm,
            gr_resize_window_for_wasm: gr_resize_window_for_wasm,
            gr_point_color_for_wasm: gr_point_color_for_wasm,
            gr_plot_for_wasm: gr_plot_for_wasm,
            gr_open_for_wasm: gr_open_for_wasm,
            gr_moveto_for_wasm: gr_moveto_for_wasm,
            gr_make_image_for_wasm: gr_make_image_for_wasm,
            gr_lineto_for_wasm: gr_lineto_for_wasm,
            gr_fill_rect_for_wasm: gr_fill_rect_for_wasm,
            gr_fill_poly_for_wasm: gr_fill_poly_for_wasm,
            gr_fill_arc_for_wasm: gr_fill_arc_for_wasm,
            gr_dump_image_width_for_wasm: gr_dump_image_width_for_wasm,
            gr_dump_image_pixel_for_wasm: gr_dump_image_pixel_for_wasm,
            gr_dump_image_height_for_wasm: gr_dump_image_height_for_wasm,
            gr_draw_str_for_wasm: gr_draw_str_for_wasm,
            gr_draw_rect_for_wasm: gr_draw_rect_for_wasm,
            gr_draw_image_for_wasm: gr_draw_image_for_wasm,
            gr_draw_char_for_wasm: gr_draw_char_for_wasm,
            gr_draw_arc_for_wasm: gr_draw_arc_for_wasm,
            gr_doc_of_state_for_wasm: gr_doc_of_state_for_wasm,
            gr_current_y_for_wasm: gr_current_y_for_wasm,
            gr_current_x_for_wasm: gr_current_x_for_wasm,
            gr_create_image_for_wasm: gr_create_image_for_wasm,
            gr_close_for_wasm: gr_close_for_wasm,
            gr_clear_for_wasm: gr_clear_for_wasm,
            gr_blit_image_for_wasm: gr_blit_image_for_wasm,
            caml_strerror: caml_strerror,
            caml_jsoo_promise_wrap: caml_jsoo_promise_wrap,
            caml_jsoo_promise_unwrap: caml_jsoo_promise_unwrap,
            caml_js_html_entities: caml_js_html_entities};
   }
   (globalThis))
({"link":[["runtime-4aab6f7d",0],["prelude-c15013c5",0],["stdlib-6bf502c5",[]],["nsdl-e89fe340",[2]],["jsoo_runtime-de5a296d",[2]],["js_of_ocaml-ca45321f",[2,4]],["dune__exe__Nsdl_web-6f37f6a3",[2,3,5]],["std_exit-e488974c",[2]],["_link_info",0],["start-5706c538",0]],"generated":(a=>{var
c=a,b=a?.module?.export||a;return{"env":{"caml_int64_create_lo_mi_hi":()=>{throw new
Error("caml_int64_create_lo_mi_hi not implemented")}},"Js_of_ocaml__Js.fragments":{"fun_call_1":(a,b)=>a(b),"get_Array":a=>a.Array,"get_Date":a=>a.Date,"get_Error":a=>a.Error,"get_JSON":a=>a.JSON,"get_Math":a=>a.Math,"get_Object":a=>a.Object,"get_RegExp":a=>a.RegExp,"get_String":a=>a.String,"get_decodeURI":a=>a.decodeURI,"get_decodeURIComponent":a=>a.decodeURIComponent,"get_encodeURI":a=>a.encodeURI,"get_encodeURIComponent":a=>a.encodeURIComponent,"get_escape":a=>a.escape,"get_isNaN":a=>a.isNaN,"get_length":a=>a.length,"get_message":a=>a.message,"get_name":a=>a.name,"get_parseFloat":a=>a.parseFloat,"get_parseInt":a=>a.parseInt,"get_stack":a=>a.stack,"get_unescape":a=>a.unescape,"js_expr_12c48ca8":()=>a,"js_expr_21711c2a":()=>b,"js_expr_26f07992":()=>null,"js_expr_28647a4c":()=>!1,"js_expr_34edcf72":()=>!0,"js_expr_ba692c1":()=>undefined,"meth_call_0_toString":a=>a.toString(),"meth_call_1_forEach":(a,b)=>a.forEach(b),"meth_call_1_keys":(a,b)=>a.keys(b),"meth_call_1_map":(a,b)=>a.map(b)},"Js_of_ocaml__Dom.fragments":{"call_1":(a,b,c)=>a.call(b,c),"get_CustomEvent":a=>a.CustomEvent,"get_defaultPrevented":a=>a.defaultPrevented,"get_length":a=>a.length,"get_nodeType":a=>a.nodeType,"get_srcElement":a=>a.srcElement,"get_target":a=>a.target,"meth_call_0_getRootNode":a=>a.getRootNode(),"meth_call_0_preventDefault":a=>a.preventDefault(),"meth_call_0_remove":a=>a.remove(),"meth_call_1_appendChild":(a,b)=>a.appendChild(b),"meth_call_1_getRootNode":(a,b)=>a.getRootNode(b),"meth_call_1_item":(a,b)=>a.item(b),"meth_call_1_removeChild":(a,b)=>a.removeChild(b),"meth_call_2_insertBefore":(a,b,c)=>a.insertBefore(b,c),"meth_call_2_replaceChild":(a,b,c)=>a.replaceChild(b,c),"meth_call_3_addEventListener":(a,b,c,d)=>a.addEventListener(b,c,d),"meth_call_3_removeEventListener":(a,b,c,d)=>a.removeEventListener(b,c,d),"new_2":(a,b,c)=>new
a(b,c),"obj_0":()=>({}),"obj_1":()=>({}),"obj_2":()=>({}),"set_bubbles":(a,b)=>a.bubbles=b,"set_cancelable":(a,b)=>a.cancelable=b,"set_capture":(a,b)=>a.capture=b,"set_composed":(a,b)=>a.composed=b,"set_detail":(a,b)=>a.detail=b,"set_once":(a,b)=>a.once=b,"set_passive":(a,b)=>a.passive=b},"Js_of_ocaml__Typed_array.fragments":{"get_ArrayBuffer":a=>a.ArrayBuffer,"get_DataView":a=>a.DataView,"get_Float32Array":a=>a.Float32Array,"get_Float64Array":a=>a.Float64Array,"get_Int16Array":a=>a.Int16Array,"get_Int32Array":a=>a.Int32Array,"get_Int8Array":a=>a.Int8Array,"get_Uint16Array":a=>a.Uint16Array,"get_Uint32Array":a=>a.Uint32Array,"get_Uint8Array":a=>a.Uint8Array,"new_1":(a,b)=>new
a(b)},"Js_of_ocaml__File.fragments":{"get_Blob":a=>a.Blob,"get_Document":a=>a.Document,"get_FileReader":a=>a.FileReader,"get_name":a=>a.name,"new_2":(a,b,c)=>new
a(b,c)},"Js_of_ocaml__Promise.fragments":{"fun_call_1":(a,b)=>a(b),"get_Promise":a=>a.Promise,"get_promise":a=>a.promise,"get_reason":a=>a.reason,"get_reject":a=>a.reject,"get_resolve":a=>a.resolve,"get_status":a=>a.status,"get_value":a=>a.value,"meth_call_0_withResolvers":a=>a.withResolvers(),"meth_call_1_all":(a,b)=>a.all(b),"meth_call_1_allSettled":(a,b)=>a.allSettled(b),"meth_call_1_any":(a,b)=>a.any(b),"meth_call_1_catch":(a,b)=>a.catch(b),"meth_call_1_finally":(a,b)=>a.finally(b),"meth_call_1_race":(a,b)=>a.race(b),"meth_call_1_reject":(a,b)=>a.reject(b),"meth_call_1_resolve":(a,b)=>a.resolve(b),"meth_call_1_then":(a,b)=>a.then(b),"meth_call_2_then":(a,b,c)=>a.then(b,c),"new_1":(a,b)=>new
a(b)},"Js_of_ocaml__Dom_html.fragments":{"fun_call_1":(a,b)=>a(b),"get_BeforeUnloadEvent":a=>a.BeforeUnloadEvent,"get_CompositionEvent":a=>a.CompositionEvent,"get_ErrorEvent":a=>a.ErrorEvent,"get_HTMLElement":a=>a.HTMLElement,"get_InputEvent":a=>a.InputEvent,"get_KeyboardEvent":a=>a.KeyboardEvent,"get_MessageEvent":a=>a.MessageEvent,"get_MouseEvent":a=>a.MouseEvent,"get_PageTransitionEvent":a=>a.PageTransitionEvent,"get_PopStateEvent":a=>a.PopStateEvent,"get_ProgressEvent":a=>a.ProgressEvent,"get_WheelEvent":a=>a.WheelEvent,"get_body":a=>a.body,"get_button":a=>a.button,"get_charCode":a=>a.charCode,"get_clientLeft":a=>a.clientLeft,"get_clientTop":a=>a.clientTop,"get_clientX":a=>a.clientX,"get_clientY":a=>a.clientY,"get_code":a=>a.code,"get_document":a=>a.document,"get_documentElement":a=>a.documentElement,"get_getContext":a=>a.getContext,"get_history":a=>a.history,"get_key":a=>a.key,"get_keyCode":a=>a.keyCode,"get_left":a=>a.left,"get_length":a=>a.length,"get_location":a=>a.location,"get_origin":a=>a.origin,"get_pageX":a=>a.pageX,"get_pageY":a=>a.pageY,"get_placeholder":a=>a.placeholder,"get_pushState":a=>a.pushState,"get_readyState":a=>a.readyState,"get_relatedTarget":a=>a.relatedTarget,"get_requestAnimationFrame":a=>a.requestAnimationFrame,"get_required":a=>a.required,"get_scrollLeft":a=>a.scrollLeft,"get_scrollTop":a=>a.scrollTop,"get_tagName":a=>a.tagName,"get_top":a=>a.top,"get_wheelDelta":a=>a.wheelDelta,"get_wheelDeltaX":a=>a.wheelDeltaX,"get_wheelDeltaY":a=>a.wheelDeltaY,"get_which":a=>a.which,"js_expr_4c8b1c6":()=>[].slice,"meth_call_0_focus":a=>a.focus(),"meth_call_0_getBoundingClientRect":a=>a.getBoundingClientRect(),"meth_call_0_getTime":a=>a.getTime(),"meth_call_0_hidePopover":a=>a.hidePopover(),"meth_call_0_showPopover":a=>a.showPopover(),"meth_call_0_stopPropagation":a=>a.stopPropagation(),"meth_call_0_toLowerCase":a=>a.toLowerCase(),"meth_call_0_togglePopover":a=>a.togglePopover(),"meth_call_1_attachShadow":(a,b)=>a.attachShadow(b),"meth_call_1_call":(a,b)=>a.call(b),"meth_call_1_charCodeAt":(a,b)=>a.charCodeAt(b),"meth_call_1_clearTimeout":(a,b)=>a.clearTimeout(b),"meth_call_1_createElement":(a,b)=>a.createElement(b),"meth_call_1_focus":(a,b)=>a.focus(b),"meth_call_1_getElementById":(a,b)=>a.getElementById(b),"meth_call_1_scrollIntoView":(a,b)=>a.scrollIntoView(b),"meth_call_1_showPopover":(a,b)=>a.showPopover(b),"meth_call_1_togglePopover":(a,b)=>a.togglePopover(b),"meth_call_2_postMessage":(a,b,c)=>a.postMessage(b,c),"meth_call_2_setTimeout":(a,b,c)=>a.setTimeout(b,c),"new_0":a=>new
a(),"obj_10":()=>({}),"obj_11":()=>({}),"obj_3":()=>({}),"obj_4":()=>({}),"obj_5":()=>({}),"obj_6":()=>({}),"obj_7":()=>({}),"obj_8":()=>({}),"obj_9":()=>({}),"set_behavior":(a,b)=>a.behavior=b,"set_block":(a,b)=>a.block=b,"set_composite":(a,b)=>a.composite=b,"set_delay":(a,b)=>a.delay=b,"set_delegatesFocus":(a,b)=>a.delegatesFocus=b,"set_direction":(a,b)=>a.direction=b,"set_duration":(a,b)=>a.duration=b,"set_easing":(a,b)=>a.easing=b,"set_endDelay":(a,b)=>a.endDelay=b,"set_fill":(a,b)=>a.fill=b,"set_force":(a,b)=>a.force=b,"set_id":(a,b)=>a.id=b,"set_inline":(a,b)=>a.inline=b,"set_iterationStart":(a,b)=>a.iterationStart=b,"set_iterations":(a,b)=>a.iterations=b,"set_left":(a,b)=>a.left=b,"set_mode":(a,b)=>a.mode=b,"set_name":(a,b)=>a.name=b,"set_preventScroll":(a,b)=>a.preventScroll=b,"set_pseudoElement":(a,b)=>a.pseudoElement=b,"set_source":(a,b)=>a.source=b,"set_targetOrigin":(a,b)=>a.targetOrigin=b,"set_timeline":(a,b)=>a.timeline=b,"set_top":(a,b)=>a.top=b,"set_transfer":(a,b)=>a.transfer=b,"set_type":(a,b)=>a.type=b},"Js_of_ocaml__Form.fragments":{"get_FormData":a=>a.FormData,"get_checked":a=>a.checked,"get_disabled":a=>a.disabled,"get_elements":a=>a.elements,"get_files":a=>a.files,"get_length":a=>a.length,"get_multiple":a=>a.multiple,"get_name":a=>a.name,"get_options":a=>a.options,"get_selected":a=>a.selected,"get_type":a=>a.type,"get_value":a=>a.value,"meth_call_0_toLowerCase":a=>a.toLowerCase(),"meth_call_1_item":(a,b)=>a.item(b),"meth_call_2_append":(a,b,c)=>a.append(b,c),"new_0":a=>new
a()},"Js_of_ocaml__Worker.fragments":{"get_Worker":a=>a.Worker,"get_data":a=>a.data,"get_importScripts":a=>a.importScripts,"get_onmessage":a=>a.onmessage,"get_postMessage":a=>a.postMessage,"meth_call_1_postMessage":(a,b)=>a.postMessage(b),"new_1":(a,b)=>new
a(b),"set_onmessage":(a,b)=>a.onmessage=b},"Js_of_ocaml__WebSockets.fragments":{"get_WebSocket":a=>a.WebSocket},"Js_of_ocaml__WebGL.fragments":{"meth_call_1_getContext":(a,b)=>a.getContext(b),"meth_call_2_getContext":(a,b,c)=>a.getContext(b,c),"obj_12":(a,b,c,d,e,f,g,h)=>({alpha:a,depth:b,stencil:c,antialias:d,premultipliedAlpha:e,preserveDrawingBuffer:f,preferLowPowerToHighPerformance:g,failIfMajorPerformanceCaveat:h})},"Js_of_ocaml__Regexp.fragments":{"get_flags":a=>a.flags,"get_index":a=>a.index,"get_length":a=>a.length,"get_source":a=>a.source,"meth_call_1_exec":(a,b)=>a.exec(b),"meth_call_1_split":(a,b)=>a.split(b),"meth_call_2_replace":(a,b,c)=>a.replace(b,c),"meth_call_2_split":(a,b,c)=>a.split(b,c),"new_2":(a,b,c)=>new
a(b,c),"set_lastIndex":(a,b)=>a.lastIndex=b},"Js_of_ocaml__Url.fragments":{"get_hash":a=>a.hash,"get_hostname":a=>a.hostname,"get_href":a=>a.href,"get_length":a=>a.length,"get_location":a=>a.location,"get_pathname":a=>a.pathname,"get_port":a=>a.port,"get_protocol":a=>a.protocol,"get_search":a=>a.search,"meth_call_0_toLowerCase":a=>a.toLowerCase(),"meth_call_1_charAt":(a,b)=>a.charAt(b),"meth_call_1_exec":(a,b)=>a.exec(b),"meth_call_1_indexOf":(a,b)=>a.indexOf(b),"meth_call_1_slice":(a,b)=>a.slice(b),"meth_call_1_split":(a,b)=>a.split(b),"meth_call_2_replace":(a,b,c)=>a.replace(b,c),"meth_call_2_slice":(a,b,c)=>a.slice(b,c),"new_1":(a,b)=>new
a(b),"new_2":(a,b,c)=>new
a(b,c),"obj_13":(a,b,c,d,e,f,g,h,i,j,k,l)=>({href:a,protocol:b,host:c,hostname:d,port:e,pathname:f,search:g,hash:h,origin:i,reload:j,replace:k,assign:l}),"set_hash":(a,b)=>a.hash=b,"set_href":(a,b)=>a.href=b,"set_lastIndex":(a,b)=>a.lastIndex=b},"Js_of_ocaml__ResizeObserver.fragments":{"get_ResizeObserver":a=>a.ResizeObserver,"meth_call_1_observe":(a,b)=>a.observe(b),"meth_call_2_observe":(a,b,c)=>a.observe(b,c),"new_1":(a,b)=>new
a(b),"obj_14":()=>({}),"obj_15":()=>({}),"set_box":(a,b)=>a.box=b},"Js_of_ocaml__PerformanceObserver.fragments":{"get_PerformanceObserver":a=>a.PerformanceObserver,"meth_call_1_observe":(a,b)=>a.observe(b),"new_1":(a,b)=>new
a(b),"obj_16":()=>({}),"set_entryTypes":(a,b)=>a.entryTypes=b},"Js_of_ocaml__Performance.fragments":{"get_performance":a=>a.performance,"meth_call_2_mark":(a,b,c)=>a.mark(b,c),"obj_17":()=>({}),"obj_18":()=>({}),"set_detail":(a,b)=>a.detail=b,"set_duration":(a,b)=>a.duration=b,"set_end":(a,b)=>a.end=b,"set_start":(a,b)=>a.start=b,"set_startTime":(a,b)=>a.startTime=b},"Js_of_ocaml__MutationObserver.fragments":{"get_MutationObserver":a=>a.MutationObserver,"meth_call_2_observe":(a,b,c)=>a.observe(b,c),"new_1":(a,b)=>new
a(b),"obj_19":()=>({}),"obj_20":()=>({}),"set_attributeFilter":(a,b)=>a.attributeFilter=b,"set_attributeOldValue":(a,b)=>a.attributeOldValue=b,"set_attributes":(a,b)=>a.attributes=b,"set_characterData":(a,b)=>a.characterData=b,"set_characterDataOldValue":(a,b)=>a.characterDataOldValue=b,"set_childList":(a,b)=>a.childList=b,"set_subtree":(a,b)=>a.subtree=b},"Js_of_ocaml__Jstable.fragments":{"get_Object":a=>a.Object,"get_length":a=>a.length,"meth_call_1_concat":(a,b)=>a.concat(b),"meth_call_1_keys":(a,b)=>a.keys(b),"meth_call_2_substring":(a,b,c)=>a.substring(b,c),"new_0":a=>new
a()},"Js_of_ocaml__Json.fragments":{"get_JSON":a=>a.JSON,"get_constructor":a=>a.constructor,"get_hi":a=>a.hi,"get_length":a=>a.length,"get_lo":a=>a.lo,"get_mi":a=>a.mi,"meth_call_1_stringify":(a,b)=>a.stringify(b),"meth_call_2_parse":(a,b,c)=>a.parse(b,c),"meth_call_2_stringify":(a,b,c)=>a.stringify(b,c)},"Js_of_ocaml__Abort.fragments":{"get_AbortController":a=>a.AbortController},"Js_of_ocaml__CSS.fragments":{"meth_call_1_test":(a,b)=>a.test(b),"new_1":(a,b)=>new
a(b)},"Js_of_ocaml__Dom_svg.fragments":{"get_SVGElement":a=>a.SVGElement,"get_document":a=>a.document,"get_tagName":a=>a.tagName,"meth_call_0_toLowerCase":a=>a.toLowerCase(),"meth_call_1_getElementById":(a,b)=>a.getElementById(b),"meth_call_2_createElementNS":(a,b,c)=>a.createElementNS(b,c)},"Js_of_ocaml__EventSource.fragments":{"get_EventSource":a=>a.EventSource,"obj_21":()=>({}),"set_withCredentials":(a,b)=>a.withCredentials=b},"Js_of_ocaml__Fetch.fragments":{"fun_call_1":(a,b)=>a(b),"fun_call_2":(a,b,c)=>a(b,c),"get_Headers":a=>a.Headers,"get_Request":a=>a.Request,"get_Response":a=>a.Response,"get_fetch":a=>a.fetch,"meth_call_2_append":(a,b,c)=>a.append(b,c),"new_0":a=>new
a(),"obj_22":()=>({}),"obj_23":()=>({})},"Js_of_ocaml__Geolocation.fragments":{"get_geolocation":a=>a.geolocation,"get_navigator":a=>a.navigator,"obj_24":()=>({})},"Js_of_ocaml__IntersectionObserver.fragments":{"get_IntersectionObserver":a=>a.IntersectionObserver,"obj_25":()=>({})},"Js_of_ocaml__Intl.fragments":{"get_Intl":a=>a.Intl,"js_expr_ba692c1":()=>undefined,"obj_26":a=>({localeMatcher:a}),"obj_27":(a,b,c,d,e,f)=>({localeMatcher:a,usage:b,sensitivity:c,ignorePunctuation:d,numeric:e,caseFirst:f}),"obj_28":(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t)=>({dateStyle:a,timeStyle:b,calendar:c,dayPeriod:d,numberingSystem:e,localeMatcher:f,timeZone:g,hour12:h,hourCycle:i,formatMatcher:j,weekday:k,era:l,year:m,month:n,day:o,hour:p,minute:q,second:r,fractionalSecondDigits:s,timeZoneName:t}),"obj_29":(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u)=>({compactDisplay:a,currency:b,currencyDisplay:c,currencySign:d,localeMatcher:e,notation:f,numberingSystem:g,signDisplay:h,style:i,unit:j,unitDisplay:k,useGrouping:l,roundingMode:m,roundingPriority:n,roundingIncrement:o,trailingZeroDisplay:p,minimumIntegerDigits:q,minimumFractionDigits:r,maximumFractionDigits:s,minimumSignificantDigits:t,maximumSignificantDigits:u}),"obj_30":(a,b)=>({localeMatcher:a,type:b}),"obj_31":(a,b,c,d)=>({localeMatcher:a,style:b,numberingSystem:c,numeric:d})},"Dune__exe__Nsdl_web.fragments":{"get_content":a=>a.content,"get_name":a=>a.name,"obj_0":(a,b)=>({loadSources:a,parseDuration:b}),"obj_1":(a,b)=>({kind:a,text:b}),"obj_2":(a,b,c)=>({a:a,b:b,medium:c}),"obj_3":(a,b)=>({due:a,label:b}),"obj_4":(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)=>({inspect:a,configure:b,invoke:c,advance:d,powerCycle:e,restartService:f,dhcpDiscover:g,ping:h,printJob:i,threadSight:j,packetSight:k,disconnect:l,reconnect:m,disconnectEndpoint:n,reconnectEndpoint:o,state:p}),"obj_5":(a,b,c,d,e)=>({clock:a,instances:b,connections:c,pending:d,log:e}),"obj_6":(a,b,c,d)=>({name:a,type:b,state:c,fields:d}),"obj_7":(a,b)=>({key:a,value:b})}}})(globalThis),"src":"nsdl_web.bc.wasm.assets"});
