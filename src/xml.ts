import { StringBuilder } from "./string_builder.ts";

interface XmlEntity {
  toString(indent: number): string;
}

export class XmlContent implements XmlEntity {
  private value: string;

  constructor(value: string) {
    this.value = value;
  }

  toString(indent: number): string {
    const sb = new StringBuilder();
    sb.append(" ".repeat(indent));
    sb.appendLine(this.value);
    return sb.toString();
  }
}

export class XmlProcessingInstruction {
  private name: string;
  private value: string;

  constructor({ name, value }: { name: string; value: string }) {
    this.name = name;
    this.value = value;
  }

  toString(_indent: number): string {
    const sb = new StringBuilder();
    sb.push("<?", this.name);
    sb.push(" ", this.value);
    sb.append("?>");

    return sb.toString();
  }
}

interface XmlNodeConstructor {
  tag: string;
  attributes?: Map<string, string>;
}

export class XmlNode implements XmlEntity {
  private children: Array<XmlEntity>;
  private tag: string;
  private attributes: Map<string, string>;

  constructor({ tag, attributes }: XmlNodeConstructor) {
    this.tag = tag;
    this.children = [];
    this.attributes = attributes ?? new Map();
  }

  addChild(child: XmlEntity | null): void {
    if (child === null) return;
    this.children.push(child);
  }

  addAttribute(key: string, value: string): void {
    this.attributes.set(key, value);
  }

  toString(indent: number): string {
    const sb = new StringBuilder();
    sb.append(" ".repeat(indent));
    sb.push("<", this.tag);

    for (const [key, value] of this.attributes.entries()) {
      sb.append(` ${key}="${value}"`);
    }

    sb.appendLine(">");

    sb.push(...this.children.map((c) => c.toString(indent + 2)));
    sb.push(" ".repeat(indent), "</", this.tag, ">");
    sb.push(`\n`);

    return sb.toString();
  }
}

export class XmlNodeBuilder {
  private node: XmlNode;

  constructor(tag: string) {
    this.node = new XmlNode({ tag });
  }

  public withAttribute(key: string, value: string): XmlNodeBuilder {
    this.node.addAttribute(key, value);
    return this;
  }

  public withChild(child: XmlEntity | null): XmlNodeBuilder {
    this.node.addChild(child);
    return this;
  }

  public withChildren(children: Array<XmlEntity | null>): XmlNodeBuilder {
    children.forEach((c) => this.node.addChild(c));
    return this;
  }

  public build(): XmlNode {
    return this.node;
  }
}

export class XmlDocument {
  private header: XmlProcessingInstruction;
  private processingInstructions: Array<XmlProcessingInstruction>;
  private children: Array<XmlEntity>;

  constructor(header?: XmlProcessingInstruction) {
    this.header = header ?? new XmlProcessingInstruction({ name: `xml`, value: `version="1.0" encoding="UTF-8"` });
    this.processingInstructions = new Array<XmlProcessingInstruction>();
    this.children = new Array<XmlEntity>();
  }

  public addProcessingInstruction(property: XmlProcessingInstruction): void {
    this.processingInstructions.push(property);
  }

  public addChild(child: XmlEntity): void {
    this.children.push(child);
  }

  public render(): string {
    return `${this.header}
${this.processingInstructions.map((p) => p.toString(0)).join("\n")}
${this.children.map((c) => c.toString(0)).join("\n")}
`.trim();
  }
}

export class XmlDocumentBuilder {
  private document: XmlDocument;

  constructor() {
    this.document = new XmlDocument();
  }

  public withHeader(header: XmlProcessingInstruction): XmlDocumentBuilder {
    this.document = new XmlDocument(header);
    return this;
  }

  public withProcessingInstruction(property: XmlProcessingInstruction): XmlDocumentBuilder {
    this.document.addProcessingInstruction(property);
    return this;
  }

  public withChild(child: XmlEntity): XmlDocumentBuilder {
    this.document.addChild(child);
    return this;
  }

  public build(): XmlDocument {
    return this.document;
  }
}
