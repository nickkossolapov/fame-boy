import {dotnet} from './_framework/dotnet.js'

const {setModuleImports, getAssemblyExports, getConfig, runMain} = await dotnet
  .withApplicationArguments("start")
  .create();

setModuleImports('main.js', {
  dom: {
    setInnerText: (selector, time) => document.querySelector(selector).innerText = time
  }
});

const config = getConfig();
const exports = await getAssemblyExports(config.mainAssemblyName);


const screenCanvas = document.getElementById("screen");
screenCanvas.width = 160;
screenCanvas.height = 144;

const ctx = screenCanvas.getContext("2d");
const imageData = ctx.createImageData(160, 144);
const buf32 = new Uint32Array(imageData.data.buffer);

const targetMCyclesPerMs = 1048.576;
let logNow = 0
let accumulator = 0.0;

function draw(newBuffer) {
  for (let i = 0; i < buf32.length; i++) {
    buf32[i] = newBuffer[i];
  }

  ctx.putImageData(imageData, 0, 0);
}

function runEmulator(last, timestamp) {
  logNow++;

  const dt = timestamp - last;
  const exactCycles = targetMCyclesPerMs * dt + accumulator;
  const cycles = Math.floor(exactCycles);
  accumulator = exactCycles - cycles;

  if (logNow === 60) {
    console.log(1000 / dt);
    logNow = 0;
  }

  const newBuffer = exports.EmulatorInterop.Step(5000);
  draw(newBuffer);

  window.requestAnimationFrame((t) => runEmulator(timestamp, t));
}


const fileOpenButton = document.getElementById("rom-file");

fileOpenButton.addEventListener("change", (ev) => {
  let file = ev.target.files[0];
  let reader = new FileReader();

  reader.onloadend = () => {
    const bytes = new Uint8Array(reader.result);
    exports.EmulatorInterop.Init(bytes);

    runEmulator(0, 0)
  }

  reader.readAsArrayBuffer(file);
});

// run the C# Main() method and keep the runtime process running and executing further API calls
await runMain();