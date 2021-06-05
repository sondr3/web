import Link from "next/link"
import { useTheme } from "next-themes"

import { MoonIcon, SunIcon } from "@/components"
import { navigation } from "@/utils"

const ThemeToggle = (): JSX.Element => {
  const { resolvedTheme, setTheme } = useTheme()

  return (
    <button
      aria-label="Toggle dark/light mode"
      onClick={() => setTheme(resolvedTheme === "dark" ? "light" : "dark")}
      className="h-7 rounded p-1 text-gray-800 bg-gray-200 dark:bg-gray-800 dark:text-gray-200"
    >
      {resolvedTheme === "dark" ? <SunIcon /> : <MoonIcon />}
    </button>
  )
}

export const Navbar = (): JSX.Element => {
  return (
    <header className="text-black dark:text-gray-400">
      <nav className="max-w-md md:max-w-4xl mx-auto px-8 py-4" aria-label="Top">
        <div className="w-full py-2 flex items-center justify-between">
          <div className="flex justify-center sm:justify-between w-full">
            <div className="flex justify-between w-full">
              <Link href="/">
                <a className="hover:text-indigo-500 dark:hover:text-indigo-200 uppercase">Home</a>
              </Link>
              <div className="sm:hidden">
                <ThemeToggle />
              </div>
            </div>
            <div className="hidden ml-10 space-x-8 sm:flex">
              <div>
                {navigation.map((link) => (
                  <Link href={link.href} key={link.name}>
                    <a className="text-base font-medium hover:text-indigo-500 dark:hover:text-indigo-200">
                      {link.name}
                    </a>
                  </Link>
                ))}
              </div>
              <ThemeToggle />
            </div>
          </div>
        </div>
        <div className="py-4 flex flex-wrap justify-center space-x-6 sm:hidden">
          {navigation.map((link) => (
            <Link href={link.href} key={link.name}>
              <a
                key={link.name}
                href={link.href}
                className="text-base font-medium hover:text-indigo-500 dark:hover:text-indigo-200"
              >
                {link.name}
              </a>
            </Link>
          ))}
        </div>
      </nav>
    </header>
  )
}
