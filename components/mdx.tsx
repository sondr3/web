/* eslint-disable react/display-name */
import { ComponentMap } from "mdx-bundler/client"
import React from "react"

const Paragraph: React.FC = (props) => {
  return <p {...props} />
}

interface HeaderProps {
  type: "h1" | "h2" | "h3" | "h4" | "h5" | "h6"
}

const Header: React.FC<HeaderProps> = ({ type, ...props }) => {
  const Tag = type as keyof JSX.IntrinsicElements
  return <Tag className="text-gray-900 dark:text-gray-300" {...props} />
}

const Code: React.FC = (props) => <code className="text-gray-900 dark:text-gray-300" {...props} />

export const components: ComponentMap = {
  h1: (props) => <Header type="h1" {...props} />,
  h2: (props) => <Header type="h2" {...props} />,
  h3: (props) => <Header type="h3" {...props} />,
  h4: (props) => <Header type="h4" {...props} />,
  h5: (props) => <Header type="h5" {...props} />,
  h6: (props) => <Header type="h6" {...props} />,
  code: Code,
  p: Paragraph,
}
