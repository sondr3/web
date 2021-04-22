import { ReactNode } from "react"

import { Menu } from "./menu"
import { Meta } from "./meta"

interface LayoutProps {
  children: ReactNode
}

export function Layout({ children }: LayoutProps): JSX.Element {
  return (
    <div className="relative bg-gray-50">
      <Meta />
      <Menu />
      <main className="lg:relative">{children}</main>
    </div>
  )
}
