import { readFile } from "./utils/fs";
import Processor, { Asciidoctor } from "asciidoctor";
import AbstractNode = Asciidoctor.AbstractNode;
import AbstractBlock = Asciidoctor.AbstractBlock;
import Html5Converter = Asciidoctor.Html5Converter;
import * as templates from "./templates/asciidoc";

export class Asciidoc {
  private baseConverter: Html5Converter;
  private asciidoc: Asciidoctor;

  constructor() {
    this.asciidoc = Processor();
    this.baseConverter = this.asciidoc.Html5Converter.create();
    this.asciidoc.ConverterFactory.register(this, ["html5"]);
  }

  async render(filepath: string): Promise<string | Error> {
    const content = await readFile(filepath);
    if (content instanceof Error) throw content;
    const rendered = this.asciidoc.load(content);

    return rendered.getContent();
  }

  protected convert<T extends AbstractNode & AbstractBlock>(node: T, transform?: string): string {
    const nodeName = transform ?? node.getNodeName();

    switch (nodeName) {
      case "paragraph":
        return templates.paragraph(node.getContent());
      case "section":
        return templates.section(node.getId(), node.getTitle(), node.getContent());
      case "preamble":
        return templates.preamble(node.getContent());
      default:
        return this.baseConverter.convert(node, transform);
    }
  }
}
