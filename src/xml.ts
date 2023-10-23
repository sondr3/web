import { StringBuilder } from "./string_builder.ts";

interface XmlEntity {
  getChildren(): Array<XmlEntity>;
  toString(indent: number): string;
}

export class XmlValue implements XmlEntity {
  private value: string;

  constructor(value: string) {
    this.value = value;
  }

  getChildren(): XmlEntity[] {
    return [this];
  }

  toString(_indent: number): string {
    return this.value;
  }
}

interface XmlPropertyConstructor {
  name: string;
  values?: Map<string, string>;
}

export class XmlProperty implements XmlEntity {
  private name: string;
  private values: Map<string, string>;

  constructor({ name, values }: XmlPropertyConstructor) {
    this.name = name;
    this.values = new Map(values ?? []);
  }

  addValue(key: string, value: string): void {
    this.values.set(key, value);
  }

  getChildren(): XmlEntity[] {
    return [this];
  }

  toString(_indent: number): string {
    const sb = new StringBuilder();
    sb.push("<?", this.name);

    Array.from(this.values.entries()).forEach(([key, value]) => sb.append(` ${key}="${value}"`));
    sb.append("?>");

    return sb.toString();
  }
}

interface XmlNodeConstructor {
  name: string;
  value?: string;
  properties?: Map<string, string>;
}

export class XmlNode implements XmlEntity {
  private children: Array<XmlEntity>;
  private name: string;
  private value: XmlValue | null;
  private properties: Map<string, string>;

  constructor({ name, value, properties }: XmlNodeConstructor) {
    this.name = name;
    this.value = value ? new XmlValue(value) : null;
    this.children = [];
    this.properties = properties ?? new Map();
  }

  setValue(value: string) {
    this.value = new XmlValue(value);
  }

  addChild(child: XmlEntity | null): void {
    if (child === null) return;
    this.children.push(child);
  }

  addProperty(key: string, value: string): void {
    this.properties.set(key, value);
  }

  getChildren(): XmlEntity[] {
    return this.children;
  }

  private property(indent: number): [string, string] {
    const start = `${" ".repeat(indent)}<${this.name}`;

    const properties = Array.from(this.properties.entries()).map(([key, value]) => `${key}="${value}"`).join(" ");
    const spacing = properties.length > 0 ? " " : "";

    const opening = `${start}${spacing}${properties}>`;
    const closing = `${this.value !== null ? "" : " ".repeat(indent)}</${this.name}>`;

    return [opening, closing];
  }

  toString(indent: number): string {
    const [opening, closing] = this.property(indent);

    const [indentChildren, children] = this.value !== null
      ? [false, this.value.toString(0)]
      : [true, this.getChildren().map((c) => c.toString(indent + 2)).join("\n")];

    const childString = indentChildren ? `\n${children}\n` : children;

    return `${opening}${childString}${closing}`;
  }
}

export class XmlNodeBuilder {
  private node: XmlNode;

  constructor(name: string) {
    this.node = new XmlNode({ name });
  }

  public setValue(value: string): XmlNodeBuilder {
    this.node.setValue(value);
    return this;
  }

  public addProperty(key: string, value: string): XmlNodeBuilder {
    this.node.addProperty(key, value);
    return this;
  }

  public addChild(child: XmlEntity | null): XmlNodeBuilder {
    this.node.addChild(child);
    return this;
  }

  public build(): XmlNode {
    return this.node;
  }
}

export class XmlDocument {
  private header: string;
  private properties: Array<XmlProperty>;
  private children: Array<XmlEntity>;

  constructor(header?: string) {
    this.header = header ?? `<?xml version="1.0" encoding="UTF-8"?>`;
    this.properties = new Array<XmlProperty>();
    this.children = new Array<XmlEntity>();
  }

  public addProperty(property: XmlProperty): void {
    this.properties.push(property);
  }

  public addChild(child: XmlEntity): void {
    this.children.push(child);
  }

  public render(): string {
    return `${this.header}
${this.properties.map((p) => p.toString(0)).join("\n")}
${this.children.map((c) => c.toString(0)).join("\n")}
`.trim();
  }
}
