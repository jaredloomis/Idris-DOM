/// IORefs.

function newIORefRaw(x) {
    return [x];
}

function readIORefRaw(ref) {
    return ref[0];
}

function writeIORefRaw(ref, x) {
    ref[0] = x;
}
