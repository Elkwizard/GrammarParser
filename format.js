const FOREGROUND_OFFSET = 30;
const BACKGROUND_OFFSET = 40;
const COLOR_MAP = {
	"black": 0,
	"red": 1,
	"green": 2,
	"yellow": 3,
	"blue": 4,
	"magenta": 5,
	"cyan": 6,
	"light gray": 7,
	"dark gray": 60,
	"light red": 61,
	"light green": 62,
	"light yellow": 63,
	"light blue": 64,
	"light magenta": 65,
	"light cyan": 66,
	"white": 67
};

function normalize(text) {
	return text.replace(/\x1b\[[\d;]+\w/g, "");
}

function color(name, text) {
	const code = COLOR_MAP[name] + FOREGROUND_OFFSET;
	return `\x1b[${code}m${normalize(text)}\x1b[0m`;
}

function background(name, text) {
	const code = COLOR_MAP[name] + BACKGROUND_OFFSET;
	return `\x1b[${code}m${normalize(text)}\x1b[0m`;
}

function indent(str) {
	return str
		.split("\n")
		.map(line => "    " + line)
		.join("\n");
}

module.exports = { color, background, indent };