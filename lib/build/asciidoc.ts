import Processor, { Asciidoctor } from "asciidoctor"
import { EitherAsync } from "purify-ts/EitherAsync"

import { Content } from "../content"
import { Site } from "../site"
import * as templates from "../templates"
import { FSError, readFile } from "../utils"
import { renderTemplate } from "./templating"

/**
 * A wrapper class around {@link https://github.com/asciidoctor/asciidoctor.js} with its
 * own HTML5 exporter.
 */
export class Asciidoc {
  private readonly baseConverter: Asciidoctor.Html5Converter
  private readonly asciidoc: Asciidoctor

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
  load(filepath: string): EitherAsync<FSError, Asciidoctor.Document> {
    return readFile(filepath)
      .map((document) => this.asciidoc.load(document))
      .mapLeft((error) => error)
  }

  /**
   * Parses a string into an {@link Asciidoctor.Document}.
   *
   * @param content - Content to parse
   * @returns A converted document
   */
  parse(content: string): Asciidoctor.Document {
    return this.asciidoc.load(content)
  }

  render(site: Site, content: Content): string {
    return renderTemplate(site, content.metadata.layout, content)
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
