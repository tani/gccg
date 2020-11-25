import Head from "next/head";
import React, { useReducer } from "react";
import GridLayout from "react-grid-layout";
import ConstituentTree from "../components/ConstituentTree";
import Derivation from "../components/Derivation";
import Grammar from "../components/Grammar";
import Sentence from "../components/Sentence";
import { reducer, withSideEffect } from "../lib/reducer";
import { AppContext, defaultAppStore } from "../lib/store";

const App: React.FC = () => {
  const [state, dispatch] = useReducer(reducer, defaultAppStore);
  return (
    <>
      <Head>
        <title>CCG Playground</title>
        <link rel="icon" href="/favicon.ico" />
      </Head>
      <AppContext.Provider value={{...state, dispatch: withSideEffect(dispatch)}}>
        <GridLayout className="layout" cols={2} rowHeight={400} width={1800}>
          <div key="sentence-form" data-grid={{ x: 0, y: 0, w: 1, h: 1 }}>
            <Sentence />
          </div>
          <div key="constituent-tree-viewer" data-grid={{ x: 0, y: 1, w: 1, h: 1 }}>
            <ConstituentTree />
          </div>
          <div key="grammar-viewer" data-grid={{ x: 1, y: 0, w: 1, h: 1 }}>
            <Grammar />
          </div>
          <div key="ccg-tree-viewer" data-grid={{ x: 1, y: 1, w: 1, h: 1 }}>
            <Derivation />
          </div>
        </GridLayout>
      </AppContext.Provider>
    </>
  );
};

export default App;
