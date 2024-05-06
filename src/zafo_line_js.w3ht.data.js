var left_id, right_id, line_id;
let lineObj = {};
var menu = document.getElementById('menu');
line_id = 90000;

function createLineElement(x, y, length, angle) {
    var line = document.createElement("div");
    line_id++;
    line.id = line_id;
    line.classList = "line";
    var styles = 'border: 0.5px solid black; ' +
        'width: ' + length + 'px; ' +
        'height: 0px; ' +
        '-moz-transform: rotate(' + angle + 'rad); ' +
        '-webkit-transform: rotate(' + angle + 'rad); ' +
        '-o-transform: rotate(' + angle + 'rad); ' +
        '-ms-transform: rotate(' + angle + 'rad); ' +
        'position: absolute; ' +
        'top: ' + y + 'px; ' +
        'left: ' + x + 'px; ' +
        '';
    line.setAttribute('style', styles);
    return line;
}

function createLine(x1, y1, x2, y2) {
    var a = x1 - x2,
        b = y1 - y2,
        c = Math.sqrt(a * a + b * b);
    var sx = (x1 + x2) / 2,
        sy = (y1 + y2) / 2;
    var x = sx - c / 2,
        y = sy;
    var alpha = Math.PI - Math.atan2(-b, a);
    return createLineElement(x, y, c, alpha);
}

function getElementOffset(element) {
    let offset = { left: 0, top: 0 };
    let current = element.offsetParent;

    offset.left += element.offsetLeft;
    offset.top += element.offsetTop;

    while (current !== null) {
        offset.left += current.offsetLeft;
        offset.top += current.offsetTop;
        current = current.offsetParent;
    }
    return offset;
}

function drawLine(id1, id2) {
    xy1 = getElementOffset(document.getElementById(id1));
    xy2 = getElementOffset(document.getElementById(id2));
    h1 = document.getElementById(id1).offsetHeight;
    h2 = document.getElementById(id2).offsetHeight;
    w1 = document.getElementById(id1).offsetWidth;
    w2 = document.getElementById(id2).offsetWidth;
    var newLine = createLine(xy1.left + w1 + 10, xy1.top + (h1 / 2), xy2.left - 10, xy2.top + (h2 / 2));
    document.body.appendChild(newLine);
    return newLine;
}

function hidden(id) {
    var parent = document.getElementById(id);
    for (var i = 0; i < parent.childNodes.length; i++) {
        if (parent.childNodes[i].nodeType === 1) {
            if (parent.classList.value == "head_close") {
                parent.classList.value = "head_open";
            } else {
                parent.classList.value = "head_close";
            }
        }
    }
}


function drawlines() {
    for (let key in lineObj) {
        if (key != null && lineObj[key].left != null) {
            if (document.getElementById(key).parentElement.className == "head_open" &&
                document.getElementById(lineObj[key].left).parentElement.className == "head_open") {
                if (lineObj[key].line == undefined) {
                    lineObj[key].line = drawLine(lineObj[key].left, key);
                }
            } else deleteLine(key);
        }
    }
}

function deleteLine(key) {

    if (lineObj[key] != undefined && lineObj[key].line != undefined) {
        lineObj[key].line.remove();
        delete lineObj[key].line;
    }
    return true;
}


function insertLineId() {
    if (left_id != null && right_id != null) {
        deleteLine(right_id);
        lineObj[right_id] = { "left": left_id, "default": null };
        left_id = setLineId(null, left_id);
        right_id = setLineId(null, right_id);
    }
}


function setLineId(new_id, old_id) {
    if (new_id != null)
        document.getElementById(new_id).style.backgroundColor = 'green';
    if (old_id != null)
        document.getElementById(old_id).style.backgroundColor = '';
    return new_id;
}



function menuClick(action) {
    if (action == "del") {
        deleteLine(right_id);
        delete lineObj[right_id];
    }
    if (action == "default") {
        var userInput;

        if (lineObj[right_id] != undefined &&
            lineObj[right_id].default != null) {
            userInput = lineObj[right_id].default;
        }

        userInput = getConfirmedInput("请输入默认值:", userInput);

        if (userInput != null) {
            deleteLine(right_id);
            lineObj[right_id] = { "default": userInput };
            right_id = setLineId(null, right_id);
        }
    }
}


function getConfirmedInput(message, defaultValue) {
    var input = prompt(message, defaultValue);
    if (input == null) {
        return null;
    }
    return input;
}

function form_submit() {
    var form = document.createElement('form');
    form.method = 'post';
    form.action = 'SAPEVENT:submit';
    for (let key in lineObj) {
        var input = document.createElement('input');
        // input.type = 'hidden';
        input.id = key;
        input.name = key;
        if (lineObj[key].default != undefined) {
            input.value = "default=" + lineObj[key].default;
        } else {
            input.value = "left=" + lineObj[key].left;
        }
        form.appendChild(input);
    }
    document.body.appendChild(form);
    form.submit();
}


document.addEventListener('click', function(event) {
    menu.style.display = "none";
    var clickedElement = event.target;
    var classname = document.getElementById(clickedElement.id).className;
    if (classname == "panel_left")
        left_id = setLineId(clickedElement.id, left_id);
    if (classname == "panel_right")
        right_id = setLineId(clickedElement.id, right_id);
    if (classname == "head_close" || classname == "head_open")
        hidden(clickedElement.id);
    insertLineId();
    clickedElement = null;
    drawlines();
});

document.addEventListener('contextmenu', function(event) {

    var classname = document.getElementById(event.target.id).className;
    if (classname !== "panel_right") {
        menu.style.display = "none";
        return;
    }
    right_id = event.target.id;
    event.preventDefault();
    menu.style.display = 'block';
    menu.style.left = `${event.clientX}px`;
    menu.style.top = `${event.clientY}px`;
});
