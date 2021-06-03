import Link from "next/link"

import { navigation } from "../utils"

export const Navbar = (): JSX.Element => {
  return (
    <header>
      <nav className="max-w-md md:max-w-4xl mx-auto px-8 py-4" aria-label="Top">
        <div className="w-full py-2 flex items-center justify-between">
          <div className="flex justify-center sm:justify-between w-full">
            <Link href="/">
              <a className="hover:text-indigo-500 uppercase">Home</a>
            </Link>
            <div className="hidden ml-10 space-x-8 sm:block">
              {navigation.map((link) => (
                <Link href={link.href} key={link.name}>
                  <a className="text-base font-medium text-black hover:text-indigo-500">{link.name}</a>
                </Link>
              ))}
            </div>
          </div>
        </div>
        <div className="py-4 flex flex-wrap justify-center space-x-6 sm:hidden">
          {navigation.map((link) => (
            <Link href={link.href} key={link.name}>
              <a key={link.name} href={link.href} className="text-base font-medium text-black hover:text-indigo-500">
                {link.name}
              </a>
            </Link>
          ))}
        </div>
      </nav>
    </header>
  )
}
