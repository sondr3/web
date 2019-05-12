import React from "react";
import "../styles/global.css";

interface Props {
  children: React.ReactNode;
}

export default function Layout({ children }: Props) {
  return (
    <React.StrictMode>
      <main>{children}</main>
    </React.StrictMode>
  );
}
