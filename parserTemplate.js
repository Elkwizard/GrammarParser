class AST {
	static REPLACE_KEY = Symbol("replace");
	static START_KEY = Symbol("start");
	static END_KEY = Symbol("end");
	static TOKENS_KEY = Symbol("tokens");
	
	#textContent;

	constructor(startIndex) {
		this[AST.START_KEY] = startIndex;
	}

	set textContent(value) {
		this.#textContent = value;
	}

	get textContent() {
		if (this.#textContent === undefined) {
			const { START_KEY, END_KEY, TOKENS_KEY } = AST;
			if (!(START_KEY in this && END_KEY in this && TOKENS_KEY in this))
				return "";
			const start = this[TOKENS_KEY][this[START_KEY]];
			const end = this[TOKENS_KEY][this[END_KEY]];
			this.#textContent = start.source.slice(
				start.position,
				end.position + end.content.length
			);
		}

		return this.#textContent;
	}
	error(message) {
		const tokens = this[AST.TOKENS_KEY];
		
		const startToken = tokens?.[this[AST.START_KEY]];
		const endToken = tokens?.[this[AST.END_KEY]]
		if (startToken) {
			startToken.error(message, endToken, this.toString());
			return true;
		}
		
		throw new Error(message);
	}

	finalize(tokens) {
		const { REPLACE_KEY, TOKENS_KEY } = AST;

		const { replace } = this;
		if (replace) return replace;

		const replacement = this[REPLACE_KEY];
		if (replacement && !Object.keys(this).length)
			return replacement;

		this[TOKENS_KEY] = tokens;
		return this;
	}

	setProperty(node, value, index) {
		const { REPLACE_KEY, END_KEY } = AST;
		const key = node.label ?? REPLACE_KEY;

		const current = this[key];
		if (current !== undefined) {
			if (key === REPLACE_KEY) this[REPLACE_KEY] = null;
			else current.push(value);
		} else this[key] = node.repeated ? [value] : value;

		this[END_KEY] = index;
	}

	copy() {
		return Object.assign(new this.constructor(), this);
	}

	clear() {
		for (const key in this)
			delete this[key];
	}
	
	from(node) {
		this.textContent = node.textContent;
		return this;
	}

	transformAll(transf) {
		return AST.transformAll(this, transf);
	}

	transform(match, transf) {
		return this.transformAll(
			node => AST.match(node, match) ? transf(node) : node
		);
	}

	forAll(fn, afterFn) {
		AST.forAll(this, fn, afterFn);
	}

	forEach(match, fn, afterFn) {
		this.forAll(node => {
			if (AST.match(node, match))
				return fn(node);
		}, afterFn ? node => {
			if (AST.match(node, match))
				afterFn(node);
		} : undefined);
	}

	#getPrintKey(key, repeat) {
		const value = this[key];
		return repeat ? value?.[repeat.index] : value; 
	}

	#print(printer, repeat) {
		if (typeof printer === "string")
			return [printer];

		if (Array.isArray(printer)) {
			const result = [];
			for (const element of printer)
				result.push(...this.#print(element, repeat));
			return result;
		}

		if (printer.key) {
			const result = this.#getPrintKey(printer.key, repeat);
			if (repeat) repeat.value = result;

			if (result !== undefined && printer.type) {
				const Type = AST[printer.type];
				if (Type && !(result instanceof Type)) {
					const ast = new Type();
					ast.replace = result;
					return [ast];
				}
			}

			return [result ?? ""];
		}

		if (printer.options) {
			for (const option of printer.options)
				if (option[0].some(key => this[key]))
					return this.#print(option[1], repeat);
			for (const option of printer.options) {
				const { key, type } = option[1];
				const ast = AST[type];
				const value = this.#getPrintKey(key, repeat);
				if (value === undefined) continue;
				if (
					(!ast && typeof value === "string") ||
					(ast && ast.replacements.includes(value.constructor.name))
				) return this.#print(option[1], repeat);
			}
			return this.#print(printer.options.at(-1)[1], repeat);
		}

		if (printer.repeat) {
			const repeat = { index: 0, value: null };
			const result = [];
			while (true) {
				const step = this.#print(printer.repeat, repeat);
				if (repeat.value === undefined) break;

				if (repeat.index && printer.delimiter) {
					repeat.index--;
					result.push(...this.#print(printer.delimiter, repeat));
					repeat.index++;
				}

				result.push(...step);
				repeat.index++;
			}
			return result;
		}
	}

	joinStrings(strs) {
		return strs.join(" ");
	}

	toString() {
		return this.joinStrings(
			this.#print(this.constructor.printer).map(String)
		);
	}

	static make = new Proxy({}, {
		get(_, key) {
			const cls = AST[key];
			const { labels } = cls;

			return (...args) => {
				const result = new cls();
				const count = Math.min(labels.length, args.length);
				for (let i = 0; i < count; i++) {
					const value = args[i];
					if (value !== undefined)
						result[labels[i]] = value;
				}
				return result;
			};
		}
	});

	static match(node, cls) {
		if (Array.isArray(cls)) return cls.some(one => AST.match(node, one));
		if (cls === AST || cls.prototype instanceof AST) return node instanceof cls;
		if (typeof cls === "string") return node.constructor.categories?.has?.(cls);
		return cls(node);
	}

	static is(value) {
		return Array.isArray(value) || value instanceof AST;
	}

	static keys(node) {
		return node.constructor.labels ?? Object.keys(node);
	}

	static transformAll(node, transf) {
		const result = transf(node);
		if (result === false) return node;
		node = result;
		if (AST.is(node)) {
			for (const key of AST.keys(node)) {
				const init = node[key];
				if (init === undefined) continue;
				const result = AST.transformAll(init, transf);
				if (result === false) delete node[key];
				else if (result !== init) {
					if (Array.isArray(node) && Array.isArray(result)) {
						node.splice(key, 1, ...result);
					} else {
						node[key] = result;
					}
				}
			}
		}
		return node;
	}

	static forAll(node, fn, afterFn) {
		if (fn(node) === false) return;
		if (AST.is(node)) {
			for (const key of AST.keys(node)) {
				const value = node[key];
				if (value === undefined) continue;
				AST.forAll(value, fn, afterFn);
			}
		}
		afterFn?.(node);
	}
}

const parse = (function () {
	$ASTExtensions
	
	class ParseError {
		constructor(message, token, stack) {
			this.message = message;
			this.token = token;
			this.stack = stack;
		}
		show() {
			const stack = "\n" + this.stack.map(line => `\tat ${line}`).reverse().join("\n");
			if (this.token)
				this.token.error(`${this.message} (at '${this.token.content}')${stack}`);
			else throw new SyntaxError(this.message + stack);
		}
	}
	
	class Graph {
		constructor(name, start, end, nodes) {
			this.name = name;
			this.start = start;
			this.end = end;
			this.nodes = nodes;
		}
		preprocess() {
			this.astClass = AST[this.name];
			for (const node of this.nodes) {
				if (node.reference)
					if (!node.terminal) node.match = definitions[node.match];
				for (const key in node.typeChoices)
					node.typeChoices[key] = node.typeChoices[key].map(index => node.to[index]);
				for (const key in node.literalChoices)
					node.literalChoices[key] = node.literalChoices[key].map(index => node.to[index]);
			}
		}
		static hydrate({ nodes, start, end, name }) {
			for (const node of nodes)
				node.to = node.to.map(inx => nodes[inx]);
			
			return new Graph(name, nodes[start], nodes[end], nodes);
		}
	}
	
	class TokenStream {
		constructor(tokens) {
			this.all = tokens;
		}
		remove(type) {
			this.all = this.all.filter(tok => tok.type !== type);
		}
	}

	$TokenStream

	const regex = $regex;
	const types = { };
	const hidden = new Set($hidden);
	for (const pair of regex) {
		const name = pair[1];
		types[name] = { name, toString: () => name };
		pair[1] = types[name];
	}
	
	const { definitions, printers, replacements } = JSON.parse($json);
	const definitionNames = $definitionNames;
	for (const name of definitionNames) {
		definitions[name] = Graph.hydrate(definitions[name]);
		AST[name].printer = printers[name];
		AST[name].replacements = replacements[name];
	}

	for (const name of definitionNames)
		definitions[name].preprocess();
	
	function parse(source, showError = true, term = "root") {
		source = source.replace(/\r/g, "");
		const stream = TokenStreamBuilder.regex(source, regex);
		
		const tokens = stream.all.filter(token => !hidden.has(token.type.name));

		let lastErrorPosition = -1;
		let lastError = null;
		let termStack = [];
	
		function error(message, index) {
			const position = index ?? 0;
			if (position > lastErrorPosition) {
				lastErrorPosition = position;
				const index = Math.min(position, tokens.length - 1);
				const token = index < 0 ? null : tokens[index];
				lastError = new ParseError(message, token, [...termStack]);
			}
	
			return null;
		}

		function makeIndent(add) {
			const colors = ["magenta", "cyan", "blue", "yellow"];
			const count = add ? makeIndent.count++ : --makeIndent.count;
			let result = "";
			for (let i = 0; i < count; i++)
				result += color(colors[i % colors.length], "│ ");
			return result;
		}
		makeIndent.count = 0;

		function matchTerm(graph, index) {
			termStack.push(graph.name);
			// console.log(`${makeIndent(true)}├ ${graph.name}?`);
			const match = matchFromNode(new graph.astClass(index), graph.start, index);
			// console.log(`${makeIndent(false)}├ ${match === null ? color("red", "no.") : color("green", "yes!")}`);
			termStack.pop();
	
			if (match === null)
				return null;
	
			match[0] = match[0].finalize(tokens);

			return match;
		}
	
		function matchFromNode(result, node, index) {
			while (true) {
				const match = matchNode(result, node, index);
		
				if (match === null)
					return null;
		
				if (node.to.length === 0) {
					if (termStack.length === 1 && match < tokens.length)
						return error(`Grammar couldn't explain complete input`, index);
					return [result, match];
				}

				const token = tokens[match];
				let { to } = node;
				if (token) {
					to = node.literalChoices[token.content];
					if (to === undefined || typeof to !== "object")
						to = node.typeChoices[token.type.name];
					if (to === undefined || typeof to !== "object")
						return error("Unexpected token", match);
					
					if (to.length > 1) {
						for (let i = 0; i < to.length; i++) {
							const subMatch = matchFromNode(result.copy(), to[i], match);
							if (subMatch !== null) return subMatch;
						}

						return null;
					}
				}
	
				node = to[to.length - 1];
				index = match;
			}
		}
	
		function matchNode(result, node, index) {
			const { match } = node;
			
			if (match === null) {
				if (node.enclose) {
					const enclosed = result.copy().finalize(tokens);
					result.clear();
					result.setProperty(node, enclosed, index);
				}

				return index;
			}
	
			const token = tokens[index];

			if (!token) 
				return error("Unexpected end of input", index);

			let value;

			if (node.reference) {
				if (node.terminal) {
					if (token.type.name === match) {
						value = token.content;
						index++;
					} else return error(`Unexpected token, expected a token of type '${match}'`, index);
				} else {
					const term = matchTerm(match, index);
					if (term === null) return null;

					value = term[0];
					index = term[1];
				}
			} else {
				if (token.content === match) {
					value = token.content;
					index++;
				} else return error(`Unexpected token, expected '${match}'`, index);
			}

			result.setProperty(node, value, index - 1);
	
			return index;
		}
	
		const result = matchTerm(definitions[term], 0);
	
		if (result === null) {
			if (showError) lastError.show();
			else throw lastError;
		}

		return result[0];
	}

	return parse;
})();