const fs = require("fs");
const path = require("path");

function readFile(path) {
	return fs.readFileSync(path, "utf-8");
}

function importJS(source, exports) {
	return eval(`${readFile(source)};({${exports.join(",")}});`);
}

const BASE_PATH = "G:/My Drive/Desktop/TokenStream/js"

const {
	TokenStreamBuilder, TokenStream, Token
} = importJS(`${BASE_PATH}/TokenStream.js`, [
	"TokenStream", "Token", "TokenStreamBuilder"
]);
const {
	color, background, indent	
} = importJS(`${BASE_PATH}/Format.js`, [
	"color", "background", "indent"
]);

const defineEnum = (...names) => Object.fromEntries(names.map(name => [name, Symbol(name)]));

const TYPE = defineEnum("NUMBER", "STRING", "COMMENT", "SYMBOL", "REGEX", "IDENTIFIER", "JS");

const [,, grammarFile, jsFile] = process.argv;

function tokenize(source) {
	source = source.replace(/\r/g, "");
	
	const SYMBOLS = [
		..."=:|;*+?[](){}<>,$@"
	].sort((a, b) => b.length - a.length);
	
	const tokens = TokenStreamBuilder.regex(source, [
		[/^(\s+|\/\*(.*?)\*\/|\/\/[^\n]*)/s, TYPE.COMMENT],
		[/^\(match, tokens\) => \{(.*?)\n\}/s, TYPE.JS],
		[/^(["'])((\\.)*(.*?))*?\1/, TYPE.STRING],
		[/^\/((\\.)*(.*?))*?\//, TYPE.REGEX],
		[/^[\w.]+/, TYPE.IDENTIFIER],
		[new RegExp(`^(${SYMBOLS.map(
			symbol => symbol.replace(/(.)/g, "\\$1")
		).join("|")})`), TYPE.SYMBOL]
	]);

	tokens.remove(TYPE.COMMENT);

	return tokens;
}

class Node {
	constructor(match = null) {
		this.match = match;
		this.reference = false;
		this.from = [];
		this.to = [];
		this.label = null;
		this.enclose = false;
		this._initialTerminals = new Set();
	}
	get initialTerminals() {
		return [...this._initialTerminals];
	}
	get initialLiterals() {
		return new Set(
			this.initialTerminals
				.filter(node => !node.reference)
				.map(node => node.match)
		);
	}
	get initialTypes() {
		return new Set(
			this.initialTerminals
				.filter(node => node.reference && node.terminal)
				.map(node => node.match)
		);
	}
	get initialTerminalTypes() {
		return new Set(
			this.initialTerminals
				.filter(node => !node.reference || node.terminal)
				.flatMap(({ match, terminal }) => terminal ? [match] : this.literalTypes(match))
		);
	}
	literalTypes(match) {
		const types = [];
		for (const key in this._types) {
			const [regex, js] = this._types[key];
			if (regex.test(match)) {
				types.push(key);
				if (!js) break;
			}
		}
		return types;
	}
	propagateTerminal(target = this) {
		if (target._initialTerminals.has(this)) return;
		target._initialTerminals.add(this);

		if (target === target._graph.start)
			for (const reference of target._graph._references)
				this.propagateTerminal(reference);

		for (let from of target.from) {
			if (from.reference && !from.terminal)
				from = this._definitions[from.match].end;
			if (!from.match) this.propagateTerminal(from);
		}
	}
	computeFastChoices() {
		{ // types
			this.typeChoices = { };

			for (let i = 0; i < this.to.length; i++) {
				const node = this.to[i];
				for (const type of node.initialTerminalTypes)
					(this.typeChoices[type] ??= []).push(i);
			}
		}

		{ // literals
			this.literalChoices = { }; 
			
			const literals = new Set(this.to.flatMap(node => [...node.initialLiterals]));
			const typeToLiterals = { };
			for (const literal of literals)
				for (const type of this.literalTypes(literal))
					(typeToLiterals[type] ??= []).push(literal);

			for (let i = 0; i < this.to.length; i++) {
				const node = this.to[i];
				for (const literal of node.initialLiterals)
					(this.literalChoices[literal] ??= []).push(i);
				for (const type of node.initialTypes)
					for (const literal of typeToLiterals[type] ?? [])
						(this.literalChoices[literal] ??= []).push(i);
			}

			for (const key in this.literalChoices)
				this.literalChoices[key] = [...new Set(this.literalChoices[key])];
		}
	}
	replace(graph) {
		for (const from of this.from)
			from.replaceConnection(this, graph.start);
		for (const to of this.to)
			to.replaceConnectionIn(this, graph.end);
	}
	becomeStart() {
		for (const from of this.from)
			from.to.splice(from.to.indexOf(this), 1);
		this.from = [];
	}
	becomeEnd() {
		for (const to of this.to)
			to.from.splice(to.from.indexOf(this), 1);
		this.to = [];
	}
	remove() {
		this.becomeStart();
		this.becomeEnd();
	}
	merge() {
		for (const from of this.from)
			from.to.splice(from.to.indexOf(this), 1, ...this.to);
		for (const to of this.to)
			to.from.splice(to.from.indexOf(this), 1, ...this.from);
	}
	validConnection(to) {
		return !!this.match || this !== to;
	}
	replaceConnection(find, replace) {
		if (find === replace) return;
		const valid = this.validConnection(replace);
		this.to.splice(this.to.indexOf(find), 1, ...(valid ? [replace] : []));
		find.from.splice(find.from.indexOf(this), 1);
		if (valid) replace.from.push(this);
	}
	replaceConnectionIn(find, replace) {
		if (find === replace) return;
		const valid = replace.validConnection(this);
		this.from.splice(this.from.indexOf(find), 1, ...(valid ? [replace] : []));
		find.to.splice(find.to.indexOf(this), 1);
		if (valid) replace.to.push(this);
	}
	connect(to) {
		if (!this.validConnection(to)) return;
		to.from.push(this);
		this.to.push(to);
	}
	forEach(fn, found = new Set()) {
		if (found.has(this)) return;
		found.add(this);
		fn(this);
		for (const dst of this.to)
			dst.forEach(fn, found);
	}
}

class Graph {
	constructor(label, start, end = start, repeated = false) {
		this.start = start;
		this.end = end;
		if (label) {
			this.start.label = label;
			this.end.label = label;
		}

		if (repeated)
			this.forEach(node => node.repeated = true);

		this.simplify();
	}
	get initialNodes() {
		const getInitialNodes = node => {
			if (node.match) return [node];
			return node.to.flatMap(getInitialNodes);
		};
		return getInitialNodes(this.start);
	}
	forEach(fn) {
		this.start.forEach(fn);
	}
	simplify() {
		const toRemove = [];
		this.forEach(node => {
			const from = node.from.length;
			const to = node.to.length;
			if (
				!node.match &&
				!node.enclose &&
				node !== this.start &&
				node !== this.end &&
				(from <= 1 || to <= 1)
			) toRemove.push(node);
		});

		for (const node of toRemove)
			node.merge();

		while (!this.start.match && this.start.to.length === 1) {
			const next = this.start.to[0];
			this.start.becomeEnd();
			this.start = next;
		}
	}
	findLabels() {
		this.labels = new Set();
		this.forEach(node => {
			if (node.label) this.labels.add(node.label);
		})
		this.labels = [...this.labels];
	}
	categorize(definitions, types) {
		this._definitions = definitions;
		this._types = types;
		const repeated = new Map();
		this.forEach(node => {
			node._definitions = definitions;
			node._types = types;
			node._graph = this;
			if (node.reference && !(node.match in definitions))
				node.terminal = true;

			if (node.label)
				repeated.set(node.label, node.repeated || repeated.get(node.label));
		});

		this.forEach(node => {
			if (node.label && repeated.get(node.label))
				node.repeated = true;
		});
		
		this._references = [];
		for (const key in definitions) {
			const graph = definitions[key];
			graph.forEach(node => {
				if (node.reference && node.match === this.name)
					this._references.push(node);
			});
		}
	}
	removeInitialRecursion() {
		const toRemove = this.initialNodes
			.filter(node => node.reference && node.match === this.name);

		const end = this.end;
		this.end = new Node();

		const start = this.start;
		this.start = new Node();
		this.start.connect(start);

		for (const node of toRemove) {
			node.reference = false;
			node.match = null;
			node.enclose = true;
			
			for (const from of [...node.from])
				from.replaceConnection(node, start);
			end.connect(node);
		}
		
		end.connect(this.end);

		this.simplify();
		
		while (true) {
			const toJoin = [];

			this.forEach(node => {
				const { to } = node;
				
				if (to.length <= 1) return;

				for (let i = 1; i < to.length; i++) {
					const a = to[i - 1];
					const b = to[i];
					if (
						a.reference !== b.reference ||
						a.match !== b.match ||
						!a.match || !b.match
					) return;
				}

				toJoin.push(node);
			});

			if (!toJoin.length) break;

			for (const node of toJoin) {
				const to = [...node.to];
				const rep = to[0];
				const joined = new Node(rep.match);
				joined.reference = rep.reference;

				for (const next of to) {
					next.match = null;
					if (next.label) next.enclose = true;
					next.replaceConnectionIn(node, joined);
				}

				node.connect(joined);
			}

			this.simplify();
		}
	}
	computeInitialTerminals() {
		this.forEach(node => {
			if (node.match && (!node.reference || node.terminal))
				node.propagateTerminal();
		});
	}
	computeFastChoices() {
		this.forEach(node => node.computeFastChoices());
	}
	flatten() {
		const nodeSet = new Set();

		function search(node) {
			if (nodeSet.has(node)) return;
			nodeSet.add(node);
			node.to.map(search);
		}

		search(this.start);

		const nodes = [...nodeSet];
		const start = nodes.indexOf(this.start);
		const end = nodes.indexOf(this.end);
		const result = [];
		for (let i = 0; i < nodes.length; i++) {
			const node = { };
			const source = nodes[i];
			for (const key in source)
				if (key[0] !== "_")
					node[key] = source[key];
			node.to = node.to.map(node => nodes.indexOf(node));
			delete node.from;
			result.push(node);
		}

		return {
			nodes: result,
			start, end,
			name: this.name
		};
	}
}

class AST {
	constructor(...children) {
		this.label = null;
		this.children = children;
		this.resolved = false;
	}
	get replaceableWith() {
		return this.children.flatMap(child => child.replaceableWith);
	}
	get labels() {
		return [...new Set(this.children.flatMap(child => child.labels))];
	}
	toPrinter() {
		return [];
	}
	resolve(definitions) {
		if (this.resolved) return this;

		for (let i = 0; i < this.children.length; i++)
			this.children[i] = this.children[i].resolve(definitions);

		this.resolved = this.children.every(child => child.resolved);

		return this;
	}
	addLabel(label) {
		for (const child of this.children)
			child.addLabel(label);
	}
	copy() {
		const result = new this.constructor();
		Object.assign(result, this);
		result.children = result.children.map(child => child.copy?.() ?? child);
		result.categories = this.categories;
		return result;
	}
	static replaceableClosure(key, replacements, found = new Set()) {
		found.add(key);

		if (key in replacements)
			for (const replace of replacements[key]) {
				if (!found.has(replace))
					AST.replaceableClosure(replace, replacements, found);
			}

		return found;
	}
	static sortOptions(printer, replacements) {
		if (typeof printer === "string" || printer.key) return;
		if (Array.isArray(printer))
			for (const el of printer)
				AST.sortOptions(el, replacements);
		else if (printer.repeat) {
			AST.sortOptions(printer.repeat, replacements);
			if (printer.delimiter)
				AST.sortOptions(printer.delimiter, replacements);
		} else if (printer.options) {
			const score = opt => opt[0].length ? 0 : 1 + (replacements[opt[1].type]?.length ?? 0);
			printer.options.sort((a, b) => score(a) - score(b));
			for (const option of printer.options)
				AST.sortOptions(option, replacements);
		}
	}
}

class ParameterAST {
	constructor(name, defaultValue = null) {
		this.name = name;
		this.defaultValue = defaultValue;
	}
}

class FunctionAST extends AST {
	constructor(...children) {
		super(...children);
		this.resolved = true;
	}
}

class VariableAST extends AST {
	resolve(definitions) {
		const name = this.children[0];
		if (!(name in definitions)) {
			for (let i = 1; i < this.children.length; i++)
				this.children[i] = this.children[i].resolve(definitions);
			return this;
		}
		
		const def = definitions[name];

		let copy;
		if (def instanceof FunctionAST) {
			const [body, ...parameters] = def.children;
			const defs = { };
			for (let i = 0; i < parameters.length; i++)
				defs[parameters[i].name] = this.children[i + 1] ?? parameters[i].defaultValue;
			copy = body.copy().resolve(defs);
		} else copy = def.copy();
		copy.categories = this.categories;

		if (this.label) copy.addLabel(this.label);

		copy = copy.resolve(definitions);

		return copy;
	}
}

class TerminalAST extends AST {
	constructor(...children) {
		super(...children);
		this.resolved = true;
	}
	get labels() {
		return this.label && this.label !== "replace" ? [this.label] : [];
	}
	addLabel(label) {
		this.label = label;
	}
}

class ReferenceAST extends TerminalAST {
	get replaceableWith() {
		return this.label && this.label !== "replace" ? [] : [this.children[0]];
	}
	toGraph() {
		const node = new Node(this.children[0]);
		node.reference = true;
		return new Graph(this.label, node);
	}
	toPrinter() {
		return {
			key: this.label ?? "replace",
			type: this.children[0]
		};
	}
}

class LiteralAST extends TerminalAST {
	get replaceableWith() {
		return [];
	}
	toGraph() {
		return new Graph(this.label, new Node(this.children[0]));
	}
	toPrinter() {
		return this.label ? {
			key: this.label
		} : this.children[0];
	}
}

class ListAST extends AST {
	constructor(required, ...children) {
		super(...children);
		this.required = required;
		this.lazy = false;
	}
	toGraph() {
		const start = new Node();
		const end = new Node();
		const [element, delim] = this.children.map(child => child.toGraph());
		start.connect(element.start);
		if (!this.required) start.connect(end);
		if (this.lazy) element.end.connect(end);
		element.end.connect(delim.start);
		if (!this.lazy) element.end.connect(end);
		delim.end.connect(element.start);
		return new Graph(this.label, start, end, true);
	}
	toPrinter() {
		const [repeat, delimiter] = this.children.map(child => child.toPrinter());
		return { repeat, delimiter };
	}
}

class OptionalAST extends AST {
	constructor(...children) {
		super(...children);
		this.lazy = false;
	}
	toGraph() {
		const start = new Node();
		const end = new Node();
		const graph = this.children[0].toGraph();
		if (this.lazy) start.connect(end);
		start.connect(graph.start);
		if (!this.lazy) start.connect(end);
		graph.end.connect(end);
		return new Graph(this.label, start, end);
	}
	toPrinter() {
		const child = this.children[0];
		const { labels } = child;
		const printer = child.toPrinter();
		const options = [[labels, printer], [[], []]];
		if (this.lazy) options.reverse();
		return { options };
	}
}

class RepeatAST extends AST {
	constructor(required, ...children) {
		super(...children);
		this.required = required;
		this.lazy = false;
	}
	toGraph() {
		const start = new Node();
		const end = new Node();

		const graph = this.children[0].toGraph();
		if (this.lazy) graph.end.connect(end);
		graph.end.connect(graph.start);
		if (!this.lazy) graph.end.connect(end);
		start.connect(graph.start);

		if (!this.required) start.connect(end);

		return new Graph(this.label, start, end, true);
	}
	toPrinter() {
		return {
			repeat: this.children[0].toPrinter()
		};
	}
}

class SequenceAST extends AST {
	toGraph() {
		const graphs = this.children.map(child => child.toGraph());
		for (let i = 0; i < graphs.length - 1; i++)
			graphs[i].end.connect(graphs[i + 1].start);
		return new Graph(this.label, graphs[0].start, graphs.at(-1).end);
	}         
	toPrinter() {
		return this.children.map(child => child.toPrinter());
	}
}

class OptionsAST extends AST {
	toGraph() {
		const start = new Node();
		const end = new Node();
		for (const option of this.children) {
			const graph = option.toGraph();
			start.connect(graph.start);
			graph.end.connect(end);
		}
		return new Graph(this.label, start, end);
	}
	toPrinter() {
		const options = this.children
			.map(child => [child.labels, child.toPrinter()]);

		if (!options.length) return null;
		if (options.length === 1) return options[0][1];

		const commonLabels = new Set(options[0][0]);
		for (let i = 1; i < options.length; i++) {
			const labels = options[i][0];
			const missing = [...commonLabels].filter(label => !labels.includes(label));
			for (const label of missing)
				commonLabels.delete(label);
		}

		for (const option of options)
			option[0] = option[0].filter(option => !commonLabels.has(option));

		return { options };
	}
}

class TokenType {
	constructor(hidden, literals, regex, js) {
		this.hidden = hidden;
		this.literals = literals;
		this.regex = regex;
		this.js = js;
		this.resolved = true;
	}
	resolve(definitions) {
		return this;
	}
}

function parse(tokens) {
	let lastAlias = null;
	let literals = new Set();

	function parseSimpleExpression(tokens, label = null) {
		if (tokens.has(":", 1)) {
			label = tokens.next(TYPE.IDENTIFIER);
			tokens.next(":");
		}

		let value;
		
		if (tokens.has("("))
			value = parseExpression(tokens.endOf("(", ")"), label);
		else {
			if (tokens.optional("$")) {
				value = new VariableAST(tokens.next(TYPE.IDENTIFIER));
			} else if (tokens.has(TYPE.IDENTIFIER)) {
				let name = tokens.next();
				if (name === "last") name = lastAlias;
				if (tokens.has("<")) {
					const args = tokens
						.endOf("<", ">")
						.delimitedList(parseExpression, ",");
					value = new VariableAST(name, ...args);
				} else value = new ReferenceAST(name);
			} else {
				const literal = JSON.parse(tokens.next(TYPE.STRING));
				literals.add(literal);
				value = new LiteralAST(literal);
			}
			value.label = label;
		}
		
		while (tokens.hasAny("*", "+", "?", "[", "{")) {
			if (tokens.optional("*"))
				value = new RepeatAST(false, value);
			else if (tokens.optional("+"))
				value = new RepeatAST(true, value);
			else if (tokens.optional("?"))
				value = new OptionalAST(value);
			else if (tokens.has("["))
				value = new ListAST(false, value, parseExpression(tokens.endOf("[", "]")));
			else value = new ListAST(true, value, parseExpression(tokens.endOf("{", "}")));
			
			if (tokens.optional("?"))
				value.lazy = true;
		}

		return value;
	}

	function parseSequence(tokens, label = null) {
		const pieces = [];
		while (tokens.length && !tokens.hasAny("|", ","))
			pieces.push(parseSimpleExpression(tokens, label));
		if (pieces.length === 1) return pieces[0];
		return new SequenceAST(...pieces);
	}

	function parseExpression(tokens, label = null) {
		if (tokens.has("literals") || tokens.has("hidden") || tokens.has(TYPE.REGEX)) {
			const hidden = tokens.optional("hidden");
			const literals = tokens.optional("literals");
			
			let regex = tokens.next(TYPE.REGEX).slice(1, -1);
			if (!literals) regex = `^(?:${regex})`;
			
			let js = null;
			if (tokens.optional("and"))
				js = tokens.next(TYPE.JS);

			return new TokenType(hidden, literals, new RegExp(regex), js);
		}

		const options = tokens.delimitedList(
			tokens => parseSequence(tokens, label), "|", ","
		);
		if (options.length === 1) return options[0];
		return new OptionsAST(...options);
	}

	const categoryStack = [];
	function parseCategoryBoundaries(tokens) {
		while (tokens.optional("@")) {
			if (tokens.optional("begin")) {
				categoryStack.push(tokens.next(TYPE.IDENTIFIER));
			} else if (tokens.optional("end")) {
				categoryStack.pop();
			}
		}
	}

	const definitions = { };
	function define(name, value) {
		definitions[name] = value;
		value.categories = [...categoryStack];
	}

	function untilDefinitionEnd(tokens) {
		const toks = [];
		while (tokens.length && !(tokens.has("=", 1) || tokens.has("@")))
			toks.push(tokens.nextToken());
		return new TokenStream(toks);
	}

	function parseDefinition(name, tokens) {
		if (tokens.has("<")) {
			const params = tokens
				.endOf("<", ">")
				.delimitedList(tok => {
					const name = tok.next(TYPE.IDENTIFIER);
					if (tok.optional("="))
						return ParameterAST(name, parseExpression(tok));
					return new ParameterAST(name);
				}, ",");
			const toks = untilDefinitionEnd(tokens);
			define(name, new FunctionAST(parseExpression(toks), ...params));
		} else if (tokens.optional("operators")) {
			const base = tokens.next(TYPE.IDENTIFIER);

			const content = tokens.endOf("{", "}");

			lastAlias = base;
			while (content.length) {
				parseCategoryBoundaries(content);
				const type = content.next(TYPE.IDENTIFIER);
				const lazy = content.optional("?");
				const name = content.next(TYPE.IDENTIFIER);
				const operator = content.endOf("(", ")");
				parseCategoryBoundaries(content);

				let operatorExpr;
				
				if (type === "custom")
					operatorExpr = parseExpression(operator);
				else {
					const sequence = [
						parseExpression(operator, "op"),
						new ReferenceAST(name),
					];

					const binary = !type.endsWith("fix");

					if (binary) sequence.unshift(new ReferenceAST(lastAlias));
					else sequence[1].label = "target";

					if (type === "left" || type === "suffix") sequence.reverse();

					if (binary) {
						sequence[0].label = "left";
						sequence.at(-1).label = "right";
					}
				
					operatorExpr = new SequenceAST(...sequence);
				}

				const def = new OptionsAST(
					operatorExpr,
					new ReferenceAST(lastAlias)
				);

				if (lazy) def.children.reverse();

				define(name, def);
				lastAlias = name;
			}

			define(name, new ReferenceAST(lastAlias));
		} else {
			const toks = untilDefinitionEnd(tokens);
			define(name, parseExpression(toks));
		}
	}

	while (tokens.length) {
		parseCategoryBoundaries(tokens);
		const name = tokens.next(TYPE.IDENTIFIER);
		tokens.next("=");
		parseDefinition(name, tokens);
	}

	for (const key in definitions) {
		const value = definitions[key];
		if (value instanceof TokenType && value.literals) {
			const { regex } = value;
			const matching = [...literals]
				.filter(literal => regex.test(literal))
				.sort((a, b) => b.length - a.length)
				.map(match => match.replace(/(\W)/g, "\\$1"));
			value.literals = false;
			value.regex = new RegExp(`^(?:${matching.join("|")})`);
		}
	}

	const result = { };
	const total = Object.keys(definitions).length;
	let count = 0;
	while (count !== total) {
		for (const key in definitions) {
			if (key in result) continue;
			const res = definitions[key].resolve(result);
			if (res.resolved) {
				count++;
				result[key] = res;
			}
		}
	}

	for (const key in result)
		if (result[key] instanceof FunctionAST)
			delete result[key];


	return result;
}

function compile(source) {
	const definitions = parse(tokenize(source));

	const categories = { };
	const regex = [];
	const json = {
		definitions: { },
		printers: { },
		replacements: { }
	};
	const types = { };
	const hidden = [];
	for (const key in definitions) {
		const value = definitions[key];
		if (value instanceof TokenType) {
			regex.push([key, value.regex, value.js]);
			types[key] = [value.regex, value.js];
			if (value.hidden) hidden.push(key);
			delete definitions[key];
		} else {
			const graph = value.toGraph();
			categories[key] = value.categories;
			graph.name = key;
			definitions[key] = graph;
			json.printers[key] = value.toPrinter();
			json.replacements[key] = value.replaceableWith;
		}
	}

	const replacementClosures = { };

	for (const key in definitions) {
		definitions[key].findLabels();
		definitions[key].removeInitialRecursion();
		replacementClosures[key] = [...AST.replaceableClosure(key, json.replacements)];
	}
	
	json.replacements = replacementClosures;

	for (const key in definitions)
		AST.sortOptions(json.printers[key], json.replacements);

	for (const key in definitions)
		definitions[key].categorize(definitions, types);

	for (const key in definitions)
		definitions[key].computeInitialTerminals();
	
	for (const key in definitions)
		definitions[key].computeFastChoices();
	
	for (const key in definitions)
		json.definitions[key] = definitions[key].flatten();
	
	// fs.writeFileSync("tree.json", JSON.stringify(json, undefined, 4), "utf-8");

	const ASTExtensions = Object.values(definitions)
		.map(graph => {
			const { name, labels } = graph;
			return `AST.${name} = class ${name} extends AST { static labels = ${JSON.stringify(labels)}; static categories = new Set(${JSON.stringify(categories[name])}); };`;
		})
		.join("\n");

	const templatePath = path.join(path.dirname(process.argv[1]), "parserTemplate.js");

	let templateJS = fs.readFileSync(templatePath, "utf-8");
	const replacements = {
		regex: `[${
			regex
				.map(([name, regex, assert]) => `[${regex}, ${JSON.stringify(name)}, ${assert}]`)
				.join(", ")
		}]`,
		definitionNames: JSON.stringify(Object.keys(definitions)),
		TokenStream: readFile(BASE_PATH + "/Format.js") + Token + TokenStreamBuilder,
		ASTExtensions,
		hidden: JSON.stringify(hidden),
		json: JSON.stringify(JSON.stringify(json)),
	};
	for (const key in replacements)
		templateJS = templateJS.replace("$" + key, replacements[key].toString());

	return templateJS;
}

fs.writeFileSync(jsFile, compile(fs.readFileSync(grammarFile, "utf-8")), "utf-8");