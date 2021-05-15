import { ReactNode } from "react"

interface Props {
  title: string
  subtitle?: string
  description?: string
  children: ReactNode
}

export function Content({ title, subtitle, description, children }: Props): JSX.Element {
  return (
    <div className="relative py-16 bg-white overflow-hidden">
      <div className="relative px-4 sm:px-6 lg:px-8">
        <div className="text-lg max-w-prose mx-auto">
          <h1>
            {subtitle && (
              <span className="block text-base text-center text-indigo-600 font-semibold tracking-wide uppercase">
                {subtitle}
              </span>
            )}
            <span className="mt-2 block text-3xl text-center leading-8 font-extrabold tracking-tight text-gray-900 sm:text-4xl">
              {title}
            </span>
          </h1>
          {description && <p className="mt-8 text-xl text-gray-500 leading-8">{description}</p>}
        </div>
        <div className="mt-6 prose prose-indigo prose-lg text-gray-500 mx-auto">{children}</div>
      </div>
    </div>
  )
}
