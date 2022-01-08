import Processor from "asciidoctor";
import { Asciidoctor } from "asciidoctor/types";
import { EitherAsync } from "purify-ts/EitherAsync";

import { readFile } from "./fs";
import { html } from "./templating.js";

export class Asciidoc {
  private readonly converter: Asciidoctor.Html5Converter;
  private readonly self: Asciidoctor;

  constructor() {
    this.self = Processor();
    this.converter = this.self.Html5Converter.create();
    this.self.ConverterFactory.register(this, ["html5"]);
  }

  /**
   * Reads and converts a file to an {@link Asciidoctor.Document}.
   *
   * @param filepath - File to open and parse
   * @returns Either a converted document or an error
   */
  load(filepath: string): EitherAsync<Error, Asciidoctor.Document> {
    return readFile(filepath)
      .map((document) => this.self.load(document))
      .mapLeft((error) => error);
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
    const nodeName = transform ?? node.getNodeName();

    switch (nodeName) {
      case "paragraph":
        return html`<p>${node.getContent()}</p>`;
      case "section":
        return html`
          <section>
            <h2 id="${node.getId()}">${node.getTitle() ?? ""}</h2>
            ${node.getContent()}
          </section>
        `;
      case "preamble":
        return html`<section class="preamble">${node.getContent()}</section>`;
      default:
        return this.converter.convert(node, transform);
    }
  }
}
