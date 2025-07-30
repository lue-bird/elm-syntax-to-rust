async function read(stream) {
    const chunks = [];
    for await (const chunk of stream) chunks.push(chunk);
    return Buffer.concat(chunks).toString('utf8');
}

const string = await read(process.stdin);

var i = string.length;
while (i--) {
    var char = string[i];
    var word = string.charCodeAt(i);
    if (0xDC00 <= word && word <= 0xDFFF) {
        i--;
        char = string[i] + char
        console.log("has surrogates " + char)
    }
}
console.log("done")
