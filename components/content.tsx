import { ReactNode } from "react"

interface Props {
  title: string
  subtitle?: string
  description?: string
  children: ReactNode
}

export function Content({ title, subtitle, description, children }: Props): JSX.Element {
  return (
    <>
      <h1>
        {subtitle && <span>{subtitle}</span>}
        <span>{title}</span>
      </h1>
      {description && <p>{description}</p>}
      <div>{children}</div>
    </>
  )
}
