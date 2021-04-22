import { ReactNode } from "react"

import { Menu } from "./menu"

interface LayoutProps {
  children: ReactNode
}

export function Layout({ children }: LayoutProps): JSX.Element {
  return (
    <div className="relative bg-gray-50">
      <Menu />
      <main className="lg:relative">{children}</main>
    </div>
  )
}
