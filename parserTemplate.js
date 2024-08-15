class AST {
	static REPLACE_KEY = Symbol("replace");
	static START_KEY = Symbol("start");
	static END_KEY = Symbol("end");
	static TOKENS_KEY = Symbol("tokens");

	constructor(startIndex) {
		this[AST.START_KEY] = startIndex;
	}

	get textContent() {
		const { START_KEY, END_KEY, TOKENS_KEY } = AST;
		if (!(START_KEY in this && END_KEY in this && TOKENS_KEY in this))
			return "";
		const start = this[TOKENS_KEY][this[START_KEY]];
		const end = this[TOKENS_KEY][this[END_KEY]];
		return start.source.slice(
			start.position,
			end.position + end.content.length
		);
	}

	finalize(tokens) {
		const { REPLACE_KEY, TOKENS_KEY } = AST;
		const replacement = this[REPLACE_KEY];
		if (replacement && !Object.keys(this).length)
			return this[REPLACE_KEY];
		delete this[REPLACE_KEY];
		this[TOKENS_KEY] = tokens;
		return this;
	}

	setProperty(node, value, index) {
		const { REPLACE_KEY, END_KEY } = AST;
		const key = node.label ?? REPLACE_KEY;
		const array = node.repeated;

		if (key in this) {
			if (key === REPLACE_KEY) this[REPLACE_KEY] = null;
			else this[key].push(value);
		} else this[key] = array ? [value] : value;

		this[END_KEY] = index;
	}

	copy() {
		return Object.assign(new this.constructor(), this);
	}

	clear() {
		for (const key in this)
			delete this[key];
	}

	transformAll(transf) {
		AST.transformAll(this, transf);
	}

	transform(match, transf) {
		this.transformAll(node => {
			if (node instanceof match) return transf(node);
			return node;
		});
	}

	forAll(fn) {
		AST.forAll(this, fn);
	}

	forEach(match, fn) {
		this.forAll(node => {
			if (node instanceof match) fn(node);
		});
	}

	static is(value) {
		return Array.isArray(value) || value instanceof AST;
	}

	static transformAll(node, transf) {
		if (AST.is(node))
			for (const key in node) {
				const result = AST.transformAll(node[key], transf);
				if (result === false) delete node[key];
				else node[key] = result;
			}
		node = transf(node);
		return node;
	}

	static forAll(node, fn) {
		if (AST.is(node))
			for (const key in node)
				AST.forAll(node[key], fn);
		fn(node);
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
			const stack = "\n" + this.stack.map(line => `\tat ${line}`).join("\n");
			if (this.token)
				this.token.error(`${this.message} (at '${this.token.content}')${stack}`);
			else throw new SyntaxError(this.message + stack);
		}
	}
	
	$Graph
	
	$Node
	
	$TokenStream
	
	const regex = $regex;
	const types = { };
	for (const pair of regex) {
		const name = pair[1];
		types[name] = { name, toString: () => name };
		pair[1] = types[name];
	}
	
	const definitions = JSON.parse($definitions);
	const definitionNames = $definitionNames;
	for (const name of definitionNames)
		definitions[name] = Graph.hydrate(definitions[name]);

	for (const name of definitionNames)
		definitions[name].preprocess(definitions, AST);
	
	function parse(source) {
		source = source.replace(/\r/g, "");
		const stream = TokenStreamBuilder.regex(source, regex);

		if ("comment" in types)
			stream.remove(types.comment);
		
		const tokens = stream.all;

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

		const OUT_OF_BOUNDS = "Unexpected end of input";
	
		function matchTerm(graph, index) {
			termStack.push(graph.name);
			const match = matchFromNode(new graph.astClass(index), graph.start, index);
			termStack.pop();
	
			if (match === null)
				return null;
	
			match[0] = match[0].finalize(tokens);
	
			return match;
		}

		const MATCH_TYPES = [
			(result, node, index) => {
				return index;
			}, (result, node, index) => {
				const enclosed = result.copy().finalize();
				result.clear();
				result.setProperty(node, enclosed, index);
				return index;
			}, (result, node, index) => {
				const term = matchTerm(node.match, index);
				if (term === null) return null;
				const end = term[1];
				result.setProperty(node, term[0], end - 1);
				return end;
			}, (result, node, index) => {
				const token = tokens[index];
				if (!token) return error(OUT_OF_BOUNDS, index);
				if (token.type.name !== node.match) return error(`Unexpected token, expected token of type '${node.match}'`, index);
				result.setProperty(node, token.content, index);
				return index + 1;
			}, (result, node, index) => {
				const token = tokens[index];
				if (!token) return error(OUT_OF_BOUNDS, index);
				if (token.content !== node.match) return error(`Unexpected token, expected '${node.match}'`, index);
				result.setProperty(node, token.content, index);
				return index + 1;
			}
		];
	
		function matchFromNode(result, node, index) {
			while (true) {
				const match = node.matchType ? matchNode(result, node, index) : index;
		
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
					to = node.literalChoices[token.content] ?? node.typeChoices[token.type.name];
					if (to === undefined)
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
			if (node.match === null) {
				const enclosed = result.copy().finalize();
				result.clear();
				result.setProperty(node, enclosed, index);
				return index;
			}

			switch (node.matchType) {
				case 2: {
					const term = matchTerm(node.match, index);
					if (term === null) return null;
					const end = term[1];
					result.setProperty(node, term[0], end - 1);
					return end;
				}

				default: {
					const token = tokens[index];
					if (!token) return "Unexpected end of input";

					const failed = (node.terminal ? token.type.name : token.content) !== node.match;
					if (failed) return null;
					// if (node.matchType === 3) {
					// 	if (token.type.name !== node.match) return error(`Unexpected token, expected token of type '${node.match}'`, index);
					// } else {
					// 	if (token.content !== node.match) return error(`Unexpected token, expected '${node.match}'`, index);
					// }

					result.setProperty(node, token.content, index);
					
					return index + 1;
				}
			}
		}
	
		// function matchNode(result, node, index) {
		// 	const { match } = node;
			
		// 	if (!match) {
		// 		if (node.enclose) {
		// 			const enclosed = result.copy().finalize();
		// 			result.clear();
		// 			result.setProperty(node, enclosed, index);
		// 		}
		// 		return index;
		// 	}
	
		// 	const token = tokens[index];

		// 	if (!token) 
		// 		return error("Unexpected end of input", index);

		// 	let value;

		// 	if (node.reference) {
		// 		if (node.terminal) {
		// 			if (token.type.name === match) {
		// 				value = token.content;
		// 				index++;
		// 			} else return error(`Unexpected token, expected a token of type '${match}'`, index);
		// 		} else {
		// 			const term = matchTerm(match, index);
		// 			if (term === null) return null;
		// 			else [value, index] = term;
		// 		}
		// 	} else {
		// 		if (token.content === match) {
		// 			value = token.content;
		// 			index++;
		// 		} else return error(`Unexpected token, expected '${match}'`, index);
		// 	}

		// 	result.setProperty(node, value, index - 1);
	
		// 	return index;
		// }
	
		const result = matchTerm(definitions.root, 0);
	
		if (result === null)
			lastError.show();
	
		return result[0];
	}

	return parse;
})();