import React from "react";
import type { AppProps } from "next/app";
import "bootstrap/dist/css/bootstrap.min.css";
import * as MathJax from "mathjax3-react"
function MyApp({ Component, pageProps }: AppProps): JSX.Element {
  return <MathJax.Provider><Component {...pageProps} /></MathJax.Provider>;
}

export default MyApp;
