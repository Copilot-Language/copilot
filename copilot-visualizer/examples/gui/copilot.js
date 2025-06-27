/* Constants */

/* Timeline dimensions */
const margin    = {top: 20, right: 20, bottom: 30, left: 90};
const width     = 960 - margin.left - margin.right;
const height    = 200 - margin.top - margin.bottom;
const rowHeight = 40;

/**
 * Position of the stream labels wrt. to the left side of the timeline.
 */
const labelPosX = -90;

/* Timeline colors */

/* Default colors for true and false (just dim green and red). */
const booleanColorDefaultTrue  = "#90EE90";
const booleanColorDefaultFalse = "#FFB6C1";

/*
 * Alternative colors for true and false, making colored backgrounds more
 * easily distinguishable in conditions with reduced color. Specifically:
 *
 * - Increase blue content in the 'false' color to increase distinction from
 * the 'true' color in protanopic (no red) and deuteranopic (no green).
 *
 * - Decrease red content in the 'false' color to increase distinction from the
 * 'true' color in achromatopsic (no color) conditions.
 *
 * This does not meaningfully address visibility considerations in reduced
 * contrast or blurry conditions.
 */
const booleanColorHighContrastTrue  = "#90EE90";
const booleanColorHighContrastFalse = "#E1B6ED";

/* Timeline state */
let svg;
let data = { adLastSample: 0, adTraceElems: [] };
let xScale;
let numRows  = 0;
let numSteps = 2;
let booleanColorTrue  = booleanColorDefaultTrue;
let booleanColorFalse = booleanColorDefaultFalse;

/**
 * Process numeric data, adding duration and y-positions to each trace element.
 */
function processNumericData(data)
{
  // Calculate bounds of stream.
  const maxValue = Math.max(...data.map(d => d.tvValue));
  const minValue = Math.min(...data.map(d => d.tvValue));

  // Vertical scale of the stream.
  const yScale = d3.scaleLinear()
                   .domain([minValue, maxValue])
                   .range([rowHeight * 0.8, rowHeight * 0.2]);

  // Return the data augmented with duration for each sample, and the vertical
  // scale.
  return data.map((d, i) => ({
    ...d,
    duration:
      i < data.length - 1 ? data[i + 1].time - d.time : numSteps + 1 - d.time,
    y: yScale(d.tvValue)
  }));
}

/**
 * Create timeline labels.
 */
function createLabels()
{
  document.querySelectorAll(".label").forEach(function(lst) {
    lst.remove();
  });

  // Add row labels.
  const labels = data.adTraceElems.map((x) => x.teName);
  svg.selectAll(".label")
     .data(labels)
     .enter()
     .append("text")
     .attr("class", "label")
     .attr("x", labelPosX)
     .attr("y", (d, i) => i * rowHeight + rowHeight/2)
     .text(d => d);
}

/**
 * Draw numeric row with stretched hexagons.
 */
function drawNumericRow(data, rowIndex)
{
  const rowG = svg.append("g")
                  .attr("class", "value_list")
                  .attr("transform", `translate(0,${rowIndex * rowHeight})`);

  const hexHeight = rowHeight * 0.8;

  time = 0;

  // Create hexagon groups.
  const hexGroups = rowG.selectAll("g")
                        .data(data)
                        .enter()
                        .append("g")
                        .attr("transform", d => `translate(${xScale(time++)},${rowHeight * 0.1})`);

  // Add hexagons.
  hexGroups.append("path")
           .attr("class", "hexagon")
           .attr("d", d => hexagonPath(xScale(1) - xScale(0), hexHeight))
           .attr("fill", "#f7fbff");

  // Add value text.
  hexGroups.append("text")
           .attr("class", "value-text")
           .attr("x", d => (xScale(1) - xScale(0)) / 2)
           .attr("y", hexHeight / 2)
           .text(d => d.tvValue);
}

/**
 * Draw boolean rows with colored backgrounds.
 */
function drawBooleanRow(data, rowIndex)
{
  const rowG = svg.append("g")
                  .attr("class", "value_list")
                  .attr("transform", `translate(0,${rowIndex * rowHeight})`);

  time = 0;

  // Draw colored backgrounds.
  rowG.selectAll("rect")
      .data(data)
      .enter()
      .append("rect")
      .attr("class", "bool-background")
      .attr("x", d => xScale(time++))
      .attr("y", 0)
      .attr("width", d => xScale(1) - xScale(0))
      .attr("height", rowHeight)
      .attr("fill", d => d.tvValue == "true" ? booleanColorTrue : booleanColorFalse);
}

/**
 * Re-draw complete timeline.
 */
function redraw()
{
  document.querySelectorAll(".value_list").forEach(function(lst) {
    lst.remove();
  });

  // Draw all rows.
  i = 0;
  data.adTraceElems.forEach(function(traceElem) {
    if (traceElem.teIsBoolean)
    {
      drawBooleanRow(traceElem.teValues, i);
    }
    else
    {
      drawNumericRow(processNumericData(traceElem.teValues), i);
    }
    i++;
  });
}

/**
 * Create SVG for the timeline.
 */
function createSVG()
{
  // Clear all existing elements.
  if (typeof svg !== 'undefined' ) { svg.selectAll("*").remove(); }

  // Calculate number of rows and columns (steps) from available data.
  numRows  = data.adTraceElems.length;
  numSteps = data.adLastSample;

  // Create SVG.
  svg = d3.select("#timeline")
          .attr("width", width + margin.left + margin.right)
          .attr("height", (rowHeight * numRows) + margin.top + margin.bottom)
          .append("g")
          .attr("transform", `translate(${margin.left},${margin.top})`);

  // Create scales.
  xScale = d3.scaleLinear()
             .domain([0, numSteps + 1])
             .range([0, width]);

  // Create x-axis with ticks at integer positions.
  const xAxis = d3.axisBottom(xScale)
                  .tickValues(Array.from({length: 11}, (_, i) => i))
                  .tickFormat('');

  createLabels();
  redraw();

  // Add the axis.
  const axisGroup = svg.append("g")
                       .attr("class", "axis")
                       .attr("transform", `translate(0,${rowHeight * numRows})`)
                       .call(xAxis);

  // After creating the axis and labels, add vertical grid lines.
  //
  // Use light gray as color, a dotted line, start from the top (negative
  // because we are going up from the axis), and end at the axis.
  const gridLines = axisGroup.selectAll(".grid-line")
                             .data(Array.from({length: 11}, (_, i) => i))
                             .enter()
                             .append("line")
                             .attr("class", "grid-line")
                             .attr("x1", d => xScale(d))
                             .attr("x2", d => xScale(d))
                             .attr("y1", -rowHeight * (numRows + 1))
                             .attr("y2", 0)
                             .style("stroke", "#ccc")
                             .style("stroke-width", "1")
                             .style("stroke-dasharray", "3,3");

  // Add offset labels.
  const timeLabels = axisGroup.selectAll(".time-label")
                              .data(Array.from({length: numRows + 1}, (_, i) => i))
                              .enter()
                              .append("text")
                              .attr("class", "time-label")
                              .attr("x", d => xScale(d + 0.5))
                              .attr("y", 25)
                              .attr("text-anchor", "middle")
                              .text(d => d + "");

  // Detect clicks on timeline.
  svg.on("click", function(event) {
    const mouseX = d3.pointer(event)[0];
    const time   = Math.floor(xScale.invert(mouseX).toFixed(2));
    const row    = Math.floor((event.offsetY - margin.top) / rowHeight);
    const label  = data.adTraceElems[row].teName;

    console.log("Label:", label , "Time:", time);
    ws.send('(Up ' + time + ', "' + label + '")');
  });

  // Detect mouse wheel on timeline.
  svg.on("wheel", function(event) {
    // Prevent default scroll behavior.
    event.preventDefault();

    const mouseX = d3.pointer(event)[0];
    const time   = Math.floor(xScale.invert(mouseX).toFixed(2));
    const row    = Math.floor((event.offsetY - margin.top) / rowHeight);
    const label  = data.adTraceElems[row].teName;
    const dir    = event.deltaY < 0 ? 'Up' : 'Down';

    ws.send('(' + dir + ' ' + time + ', "' + label + '")');

    console.log ( "Label:", label, "Time:", time, "Direction:", dir);
  });
}

/**
 * Add a step to the simulation.
 */
function stepUp()
{
  ws.send('(StepUp, "")');
}

/**
 * Reduce the number of steps of the simulation.
 */
function stepDown()
{
  ws.send('(StepDown, "")');
}

/**
 * Open a dialog requesting information to add a new stream to the simulation.
 */
function addStream()
{
  const popup = document.getElementById("popup");
  popup.classList.remove("hidden");
}

/**
 * Cancel dialog requesting information to add a new stream to the simulation.
 */
function closePopup()
{
  const popup = document.getElementById("popup");
  popup.classList.add("hidden");
}

/**
 * Add a new stream to the simulation. The definition provided must be correct
 * in the context of the current simulation, and the name given to the new
 * stream must be valid in Haskell, unique in the current context, and not
 * clash with any existing names.
 */
function addStreamSubmit()
{
  const popup = document.getElementById("popup");
  const inputName = document.getElementById("inputName").value;
  const inputExpr = document.getElementById("inputExpr").value;
  ws.send('(AddStream "' + inputName + '" "(' + inputExpr + ')", "")');
  popup.classList.add("hidden");
}

/**
 * Toggle the color scheme of the timeline.
 */
function toggleColors()
{
  const highContrastBtn  = document.getElementById('high_contrast');
  if (highContrastBtn.checked) {
    booleanColorTrue  = booleanColorHighContrastTrue;
    booleanColorFalse = booleanColorHighContrastFalse;
  } else {
    booleanColorTrue  = booleanColorDefaultTrue;
    booleanColorFalse = booleanColorDefaultFalse;
  }

  createSVG();
}

/* Attach event handlers to UI elements */

// Somehow this does not work if we match the svg element directly, so we match
// the parent div.
document.getElementById("copyPNG").addEventListener("click", () => copyPNG("#timeline-parent"));

document.getElementById("copySVG").addEventListener("click", () => copySVG("#timeline"));

// Somehow this does not work if we match the svg element directly, so we match
// the parent div.
document.getElementById("downloadPNG").addEventListener("click", () => downloadPNG("#timeline-parent", "timeline"));

document.getElementById("downloadSVG").addEventListener("click", () => downloadSVG("#timeline", "timeline"));

document.getElementById("stepUp").addEventListener("click", stepUp);
document.getElementById("stepDown").addEventListener("click", stepDown);
document.getElementById("addStream").addEventListener("click", addStream);
document.getElementById("closePopup").addEventListener("click", closePopup);
document.getElementById("submitPopup").addEventListener("click", addStreamSubmit);

// Adjust colors using high-contrast when applicable.
document.getElementById('high_contrast').addEventListener('change', toggleColors);

createSVG();

// Communicate with the backend instance of Copilot via a websocket.
let ws = new WebSocket('ws://localhost:9160');

ws.onmessage = function(event) {
  data = JSON.parse(event.data);
  createSVG();
};

ws.onclose = function() {
  console.log('Disconnected from server');
};

ws.onerror = function(error) {
  console.log('Error: ' + error.message);
};

/* Auxiliary functions */

/**
 * Create an hexagon path (used for cells or samples).
 */
function hexagonPath(width, height)
{
  return `
    M ${width-10},0
    L ${width},${height/2}
    L ${width-10},${height}
    L ${10},${height}
    L ${0},${height/2}
    L ${10},0
    Z
  `;
}

/**
 * Download a file containing a given text.
 */
function download(filename, text)
{
  var element = document.createElement('a');
  element.setAttribute('href', 'data:text/plain;charset=utf-8,' + encodeURIComponent(text));
  element.setAttribute('download', filename);

  element.style.display = 'none';
  document.body.appendChild(element);

  element.click();

  document.body.removeChild(element);
}

/**
 * Copy element as SVG to clipboard as SVG.
 *
 * @param {svg} queryString - Query string referring to an SVG element in the
 *                            current document.
 */
function copySVG(queryString)
{
  const mySVG = document.querySelector(queryString);
  const data  = new XMLSerializer().serializeToString(mySVG);

  navigator.clipboard.writeText(data).then(
    () => {
      console.log("SVG copied to clipboard");
    },
    (err) => {
      console.log("Failed to copy SVG to clipboard");
    }
  );
}

/**
 * Copy timeline to clipboard as PNG.
 *
 * @param {string} queryString - Query string referring to element in the
 *                               current document.
 */
function copyPNG(queryString)
{
  const elem = document.querySelector(queryString);
  html2canvas(elem).then(function(canvas) {
    canvas.toBlob(function(blob) {
      navigator.clipboard
        .write([
          new ClipboardItem(
            Object.defineProperty({}, blob.type, {
              value: blob,
              enumerable: true
            })
          )
        ]).then(function() {
          console.log("Copied to clipboard");
        });
    });
  });
}

/**
 * Download screenshot of an element as a PNG file.
 *
 * @param {string} queryString - Query string referring to element in the
 *                               current document.
 *
 * @param {string} defaultFilename - Default file name used in the save file
 *                                   dialog.
 */
function downloadPNG(queryString, defaultFilename)
{
  const mySVG = document.querySelector(queryString);
  html2canvas(mySVG).then(function(canvas) {
    var a      = document.createElement('a');
    a.href     = canvas.toDataURL();
    a.download = defaultFilename + ".png";
    a.click();
  });
}

/**
 * Download screenshot of timeline as SVG file.
 *
 * @param {string} queryString - Query string referring to element in the
 *                               current document.
 *
 * @param {string} defaultFilename - Default file name used in the save file
 *                                   dialog.
 */
function downloadSVG(queryString, defaultFilename)
{
  const mySVG = document.querySelector(queryString);
  const data  = new XMLSerializer().serializeToString(mySVG);
  download(defaultFilename + ".svg", data);
}
