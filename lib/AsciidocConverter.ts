import { TemplateEngine } from "./template";
import { readFile } from "./utils/fs";
import Processor, { Asciidoctor } from "asciidoctor";
import AbstractNode = Asciidoctor.AbstractNode;
import AbstractBlock = Asciidoctor.AbstractBlock;
import Html5Converter = Asciidoctor.Html5Converter;

export class AsciidocConverter {
  private baseConverter: Html5Converter;
  private asciidoc: Asciidoctor;
  private templateEngine: TemplateEngine;

  constructor() {
    this.templateEngine = new TemplateEngine();
    this.asciidoc = Processor();
    this.baseConverter = this.asciidoc.Html5Converter.create();
    this.asciidoc.ConverterFactory.register(this, ["html5"]);
  }

  async renderAsciidoc(filepath: string): Promise<string | Error> {
    const content = await readFile(filepath);
    if (content instanceof Error) throw content;
    const rendered = this.asciidoc.load(content);

    return rendered.getContent();
  }

  protected convert<T extends AbstractNode & AbstractBlock>(node: T, transform?: string): string {
    const nodeName = transform ?? node.getNodeName();

    switch (nodeName) {
      case "paragraph":
        return this.templateEngine.renderAsciidocTemplate("paragraph", { content: node.getContent() });
      case "section":
        return this.templateEngine.renderAsciidocTemplate("section", {
          title: node.getTitle(),
          titleId: node.getId(),
          content: node.getContent(),
        });
      case "preamble":
        return this.templateEngine.renderAsciidocTemplate("preamble", {
          content: node.getContent(),
        });
      default:
        return this.baseConverter.convert(node, transform);
    }
  }
}
