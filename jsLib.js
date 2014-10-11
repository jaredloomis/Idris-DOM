/** @constructor */
function JsPair(a, b) {
    this.first = a;
    this.second = b;

    this.getFirst = function() {
        return this.first;
    };
    this.getSecond = function(){
        return this.second;
    };
}

function addEventListenerAccumPair(element, eventType, func, val) {
    element.addEventListener(eventType, function(eventVal) {
            makeAccumFuncPair(func, val, eventVal);
        });
}

function makeAccumFuncPair(func, val, eventVal) {
    if(typeof makeAccumFuncPair.accum == 'undefined')
        makeAccumFuncPair.accum = val;
    makeAccumFuncPair.accum = func(new JsPair(eventVal, makeAccumFuncPair.accum));
}

/*
 * Get the style of an element, but fill in any
 * empty properties of "element.style" with the
 * corresponding property from "document.getComputedStyle".
 */
function getStyleObject(elem) {
    if(typeof elem.style.objectized === 'undefined' || !elem.style.objectized)
    {
        if(elem)
        {
            var computed = getComputedStyle(elem, null);
            if(computed)
            {
                var len = computed.length;
                for(var i=0; i < len; ++i) {
                    var propName = computed[i];
                    if(elem.style[propName] === null || !elem.style[propName].length)
                    {
                        var val = computed.getPropertyValue(propName);
                        elem.style[propName] = val;
                    }
                }
            }
            elem.style.objectized = true;
            return elem.style;
        }
        console.log('getStyleObject recieved ' + elem + '.');
        return elem;
    }
    return elem.style
}

function setTimeoutIdris(func, milliseconds) {
    var callFunc = function() {
        // For some reason, calling "func()" doesn't work,
        // so I send the function a dummy arg.
        func('setTimeoutIdris_dummy_string');
    };
    return setTimeout(callFunc, milliseconds);
}

function requestAnimationFrameIdris(func, element) {
    var callFunc = function() {
        // For some reason, calling "func()" doesn't work,
        // so I send the function a dummy arg.
        func('requestAnimationFrameIdris_dummy_string');
    };
    if(window.requestAnimationFrame)
        return requestAnimationFrame(callFunc, element);
    console.log("requestAnimationFrame not supported. using setTimeout.");
    return setTimeout(callFunc, 16);
}

function setIntervalIdris(func, milliseconds) {
    var callFunc = function() {
        // For some reason, calling "func()" doesn't work,
        // so I send the function a dummy arg.
        func('setTimeoutIdris_dummy_string');
    };
    return setInterval(callFunc, milliseconds);
}

function setIntervalIdris2(initial, func, milliseconds) {
    if(typeof setIntervalIdris2.accum == 'undefined')
        setIntervalIdris2.accum = initial;
    var callFunc = function() {
        setIntervalIdris2.accum = func(
                new JsPair(setIntervalIdris2.interval, setIntervalIdris2.accum));
    };

    setIntervalIdris2.interval = window.setInterval(callFunc, milliseconds);
    return setIntervalIdris2.interval;
}
