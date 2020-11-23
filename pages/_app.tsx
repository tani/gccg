import React from "react";
import type { AppProps } from "next/app";
import "../styles/dagre.css"
import "../styles/mathjax.css"
import "react-grid-layout/css/styles.css"
import "react-resizable/css/styles.css"
import "xp.css"

function MyApp({ Component, pageProps }: AppProps): JSX.Element {
  return <Component {...pageProps} />;
}

export default MyApp;
