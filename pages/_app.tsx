import { configureStore } from "@reduxjs/toolkit";
import type { AppProps } from "next/app";
import React from "react";
import { Provider } from "react-redux";
import "../styles/dagre.css";
import "react-grid-layout/css/styles.css";
import "react-resizable/css/styles.css";
import "xp.css";
import { initialState, slice } from "../lib/slice";


function MyApp({ Component, pageProps }: AppProps): JSX.Element {
  const store = configureStore({reducer: slice.reducer, preloadedState: initialState, devTools: true });
  return (
    <Provider store={store}>
      <Component {...pageProps} />
    </Provider>
  );
}

export default MyApp;
