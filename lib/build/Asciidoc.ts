import { readFile } from "../utils/"
import Processor, { Asciidoctor } from "asciidoctor"
import * as templates from "../templates/"

/**
 * A wrapper class around {@link https://github.com/asciidoctor/asciidoctor.js} with its
 * own HTML5 exporter.
 */
export class Asciidoc {
  private baseConverter: Asciidoctor.Html5Converter
  private asciidoc: Asciidoctor

  constructor() {
    this.asciidoc = Processor()
    this.baseConverter = this.asciidoc.Html5Converter.create()
    this.asciidoc.ConverterFactory.register(this, ["html5"])
  }

  /**
   * Reads and converts a file to an {@link Asciidoctor.Document}.
   *
   * @param filepath - File to open and parse
   * @returns Either a converted document or an error
   */
  async load(filepath: string): Promise<Asciidoctor.Document | Error> {
    const content = await readFile(filepath)
    if (content instanceof Error) throw content
    return this.asciidoc.load(content)
  }

  /**
   * Custom converter to override the default HTML5 templates used by Asciidoctor
   * to render HTML.
   *
   * @param node - AST node to render
   * @param transform - Only defined when node is a Document
   * @returns The converted node
   */
  protected convert<T extends Asciidoctor.AbstractNode & Asciidoctor.AbstractBlock>(
    node: T,
    transform?: string,
  ): string {
    const nodeName = transform ?? node.getNodeName()

    switch (nodeName) {
      case "paragraph":
        return templates.paragraph(node.getContent())
      case "section":
        return templates.section(node.getId(), node.getTitle() ?? "", node.getContent())
      case "preamble":
        return templates.preamble(node.getContent())
      default:
        return this.baseConverter.convert(node, transform)
    }
  }
}
