"use strict";
function popCount(n) {
    n = n - ((n >> 1) & 0x55555555);
    n = (n & 0x33333333) + ((n >> 2) & 0x33333333);
    return ((n + (n >> 4) & 0xF0F0F0F) * 0x1010101) >> 24;
}
function bit(bit) {
    return 1 << bit;
}
function bitTest(i, bit) {
    return (i & (1 << bit)) !== 0;
}
function arrayRead(array, index) {
    if (index < array.length) {
        return array[index];
    }
    else {
        throw new Error(`Tried to read past end of array (${index} > ${array.length})`);
    }
}
function hex(i, digits) {
    return `0x${pad(i.toString(16), digits, '0').toUpperCase()}`;
}
function hexN(i, digits) {
    return pad(i.toString(16), digits, '0').toUpperCase();
}
function hexN_LC(i, digits) {
    return pad(i.toString(16), digits, '0');
}
function bin(i, digits) {
    return `0b${pad(i.toString(2), digits, '0').toUpperCase()}`;
}
function binN(i, digits) {
    return pad(i.toString(2), digits, '0').toUpperCase();
}
function pad(n, width, z) {
    z = z || '0';
    n = n + '';
    return n.length >= width ? n : z.repeat(width - n.length) + n;
}
function r_pad(n, width, z) {
    z = z || '0';
    n = n + '';
    return n.length >= width ? n : n + z.repeat(width - n.length);
}
