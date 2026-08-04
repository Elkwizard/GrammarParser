class Token {
	constructor(content, type, position = 0, source = content, filename) {
		this.content = content;
		this.type = type;
		this.position = position;
		this.source = source;
		this.filename = filename;
	}
	
	get location() {
		if (!this._location) {
			const before = this.source.slice(0, this.position);
			const line = (before.match(/\n/g)?.length ?? 0) + 1;
			const column = before.match(/.*$/)[0].length;
			this._location = { line, column, filename: this.filename };
		}

		return this._location;
	}
	
	get lineNumber() {
		return this.location.line;
	}

	plus(token, type) {
		return new Token(
			this.content + token.content,
			type ?? this.type,
			this.position,
			this.source
		);
	}

	error(message, endToken = null, currentContent = null) {
		endToken ??= this;
		
		const normalize = str => str.replace(/\x1b\[.*?[a-z]|\s*/ig, "");
		
		const prefix = this.source.slice(0, this.position);
		const suffix = this.source.slice(endToken.position + endToken.content.length);
		const sourceContent = this.source.slice(this.position, endToken.position + endToken.content.length);
		if (currentContent && normalize(currentContent) === normalize(sourceContent))
			currentContent = "";
		const akaMarker = currentContent ? " AKA " + background("blue", currentContent) : "";
		const content = background("red", sourceContent) + akaMarker;
		const newSource = prefix + content + suffix;
		const lines = newSource.split("\n");

		const indexOf = pos => {
			let index = 0;
			for (let i = 0; i < pos; i++)
				if (newSource[i] === "\n") index++;
			
			return index;
		};

		
		const startIndex = indexOf(this.position);
		const endIndex = indexOf(this.position + content.length);

		const firstShownIndex = Math.max(0, startIndex - 1);
		const endShownIndex = Math.min(lines.length, endIndex + 2);

		const excerptLines = lines.slice(
			firstShownIndex, endShownIndex
		);

		const lineNumbers = excerptLines.map((_, i) => String(i + firstShownIndex + 1));
		const maxWidth = lineNumbers.at(-1).length;

		const excerpt = excerptLines
			.map((line, i) => `${lineNumbers[i].padStart(maxWidth)} │ ${line}`)
			.join("\n")
			.replace(/\t/g, "    ");

		const barPrefix = "═".repeat(maxWidth + 1);
		const bar = "═".repeat(40);
		const lineBlock = `${barPrefix}╤${bar}\n${excerpt}\x1b[0m\n${barPrefix}╧${bar}`;
		const columnIndex = this.position - newSource.lastIndexOf('\n', this.position) - 1;
		const filename = this.filename ? `in ${this.filename}:\n` : "";
		const position = `${filename}line ${startIndex + 1}, column ${columnIndex + 1}:`;
		const output = `\n\n${lineBlock}\n${position}\n${message}`;
		throw new Error(output);
		// throw new SyntaxError(message + "\n\n" + excerpt);
	}

	toString() {
		return `(${this.type.toString()}: ${color("blue", this.content)})`;
	}
}

class TokenStream {
	constructor(tokens = []) {
		this.tokens = [...tokens].reverse();
	}

	get length() {
		return this.tokens.length;
	}

	get all() {
		return [...this.tokens].reverse();
	}

	copy() {
		return new TokenStream(this.all);
	}

	prepend(token) {
		this.tokens.push(token);
	}

	has(content, index = 0) {
		if (index >= this.tokens.length)
			return false;
		
		const token = this.tokens[this.tokens.length - index - 1];
		if (typeof content === "string")
			return token.content === content;
		return token.type === content;
	}

	hasAny(...options) {
		let index = 0;
		if (typeof options[options.length - 1] === "number")
			index = options.pop();

		for (let i = 0; i < options.length; i++)
			if (this.has(options[i], index))
				return true;

		return false;
	}

	get(index, quiet) {
		return this.getToken(index, quiet).content;
	}

	getToken(index = 0, quiet = false) {
		if (index >= this.tokens.length && !quiet)
			throw new RangeError("Desired index is out of bounds");
		return this.tokens[this.tokens.length - index - 1];
	}

	skip(amount) {
		if (amount > this.tokens.length)
			throw new RangeError("Cannot skip over tokens in an empty stream");
		this.tokens.length -= amount;
	}
	
	skipAll(tok) {
		while (this.has(tok)) this.next();
	}

	remove(content) {
		if (typeof content === "string")
			this.tokens = this.tokens.filter(token => token.content !== content);
		else
			this.tokens = this.tokens.filter(token => token.type !== content);
	}

	nextToken() {
		if (!this.tokens.length)
			throw new RangeError("Cannot advance an empty stream");

		return this.tokens.pop();
	}

	next(expected) {
		if (!this.tokens.length)
			throw new RangeError("Cannot advance an empty stream");
		
		const token = this.tokens.pop();

		if (expected !== undefined) {
			if (typeof expected === "string") {
				if (token.content !== expected)
					token.error(`Unexpected token '${token.content}', expected '${String(expected)}'`);
			} else {
				if (token.type !== expected)
					token.error(`Unexpected token '${token.content}', expected token of type '${String(expected)}'`);
			}
		}

		return token.content;
	}

	optional(content) {
		if (this.has(content)) {
			this.next();
			return true;
		}

		return false;
	}

	until(tok) {
		const result = [];
		while (this.tokens.length && !this.has(tok))
			result.push(this.nextToken());
		return new TokenStream(result);
	}

	endOf(open, close) {
		const result = [];

		this.until(open);
		if (!this.tokens.length)
			throw new RangeError(`The specified boundaries "${open}${close}" don't exist`);
		this.next();

		let depth = 1;
		while (this.tokens.length && depth) {
			if (this.has(open)) depth++;
			if (this.has(close)) depth--;
			result.push(this.nextToken());
		}

		result.pop();
		return new TokenStream(result);
	}

	delimitedList(parseItem, delimiter, interrupt) {
		const results = [];

		while (this.tokens.length) {
			results.push(parseItem(this));

			if (interrupt !== undefined && this.has(interrupt))
				break;

			if (this.tokens.length)
				this.next(delimiter);
		}

		return results;
	}

	toString() {
		return this.all.join(" ");
	}
}

class TokenStreamBuilder {
	constructor(source, filename) {
		this.source = source;
		this.filename = filename;
		this.index = 0;
		this.tokens = [];
	}

	get stream() {
		return new TokenStream(this.tokens);
	}

	append(content, type) {
		const position = this.source.indexOf(content, this.index);
		this.index = position + content.length;
		this.tokens.push(new Token(content, type, position, this.source, this.filename));
	}

	static regex(source, regexes, filename) {
		const builder = new TokenStreamBuilder(source, filename);

		tokenize: while (source.length) {
			for (let i = 0; i < regexes.length; i++) {
				const [regex, type, assert] = regexes[i];
				if (regex.test(source)) {
					const content = source.match(regex)[0];
					if (assert && !assert(content, builder.tokens)) continue;
					builder.append(content, type);
					source = source.slice(content.length);
					continue tokenize;
				}
			}

			if (source.length) throw new SyntaxError(`Tokenization failed at position ${builder.index}: '${source[0]}'`);
		}

		return builder.stream;
	}
}

module.exports = {
	TokenStreamBuilder,
	TokenStream,
	Token
};