class AST {
	static REPLACE_KEY = Symbol("replace");

	finalize() {
		const { REPLACE_KEY } = AST;
		const replacement = this[REPLACE_KEY];
		if (replacement && !Object.keys(this).length)
			return this[REPLACE_KEY];
		delete this[REPLACE_KEY];
		return this;
	}
	setProperty(node, value) {
		const { REPLACE_KEY } = AST;
		const key = node.label ?? REPLACE_KEY;
		const array = node.repeated;

		if (key in this) {
			if (key === REPLACE_KEY) this[REPLACE_KEY] = null;
			else this[key].push(value);
		} else this[key] = array ? [value] : value;
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
	static transformAll(node, transf) {
		if (typeof node === "object")
			for (const key in node) {
				const result = AST.transformAll(node[key], transf);
				if (result === false) delete node[key];
				else node[key] = result;
			}
		node = transf(node);
		return node;
	}
	static forAll(node, fn) {
		if (typeof node === "object")
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
		definitions[name].preprocess(definitions, types, AST);
	
	function parse(source) {
		source = source.replace(/\r/g, "");
		const stream = TokenStreamBuilder.regex(source, regex);

		if ("comment" in types)
			stream.remove(types.comment);
		
		const tokens = stream.all;

		let lastErrorPosition = -1;
		let lastError = null;
	
		function error(message, index, termStack) {
			const position = index ?? 0;
			if (position > lastErrorPosition) {
				lastErrorPosition = position;
				const index = Math.min(position, tokens.length - 1);
				const token = index < 0 ? null : tokens[index];
				lastError = new ParseError(message, token, [...termStack]);
			}
	
			return null;
		}
	
		function matchTerm(graph, index, termStack = []) {
			termStack.push(graph.name);
			const match = matchFromNode(new graph.astClass(), graph.start, index, termStack);
			termStack.pop();
	
			if (match === null)
				return null;
	
			match[0] = match[0].finalize();
	
			return match;
		}
	
		function matchFromNode(result, node, index, termStack) {
			while (true) {
				const match = matchNode(result, node, index, termStack);
		
				if (match === null)
					return null;
		
				if (node.to.length === 0) {
					if (termStack.length === 1 && match < tokens.length)
						return error(`Grammar couldn't explain complete input`, index, termStack);
					return [result, match];
				}
			
				if (node.to.length > 1) {
					for (const to of node.to) {
						const subMatch = matchFromNode(result.copy(), to, match, termStack);
						if (subMatch !== null) return subMatch;
					}
					
					return null;
				}

				node = node.to[0];
				index = match;
			}
		}
	
		function matchNode(result, node, index, termStack) {
			const { match } = node;
			
			if (!match) {
				if (node.enclose) {
					const enclosed = result.copy().finalize();
					result.clear();
					result.setProperty(node, enclosed);
				}
				return index;
			}
	
			let value;
	
			const token = tokens[index];

			if (!token) 
				return error("Unexpected end of input", index, termStack);

			if (node.reference) {
				if (node.terminal) {
					if (token.type === match) {
						value = token.content;
						index++;
					} else return error(`Unexpected token, expected a token of type '${match}'`, index, termStack);
				} else {
					const term = matchTerm(match, index, termStack);
					if (term === null) return null;
					else [value, index] = term;
				}
			} else {
				if (token.content === match) {
					value = token.content;
					index++;
				} else return error(`Unexpected token, expected '${match}'`, index, termStack);
			}
	
			result.setProperty(node, value);
	
			return index;
		}
	
		const result = matchTerm(definitions.root, 0);
	
		if (result === null)
			lastError.show();
	
		return result[0];
	}

	return parse;
})();