import Link from "next/link"

import { navigation, socials } from "@/utils"

export const Footer = (): JSX.Element => {
  return (
    <footer>
      <div className="max-w-7xl mx-auto py-12 px-4 overflow-hidden sm:px-6 lg:px-8">
        <nav className="-mx-5 -my-2 flex flex-wrap justify-center" aria-label="Footer">
          {navigation.map((item) => (
            <div key={item.name} className="px-5 py-2">
              <Link href={item.href}>
                <a className="text-base text-gray-500 hover:text-gray-900">{item.name}</a>
              </Link>
            </div>
          ))}
        </nav>
        <div className="mt-8 flex justify-center space-x-6">
          {socials.map(({ name, href, Icon }) => (
            <a
              key={name}
              href={href}
              className="text-gray-400 hover:text-gray-500"
              rel="noopener noreferrer"
              target="_blank"
            >
              <span className="sr-only">{name}</span>
              <Icon aria-hidden="true" />
            </a>
          ))}
        </div>
        <p className="mt-8 text-center text-base text-gray-400">
          License{" "}
          <a rel="noopener noreferrer" target="blank" href="https://creativecommons.org/licenses/by-sa/4.0/">
            CC BY-SA 4.0
          </a>{" "}
          — Sondre Nilsen
        </p>
      </div>
    </footer>
  )
}
