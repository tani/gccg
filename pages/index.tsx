import Head from "next/head";
import React from "react";
import GridLayout from "react-grid-layout";
import ConstituentTree from "../components/ConstituentTree";
import Derivation from "../components/Derivation";
import Grammar from "../components/Grammar";
import Sentence from "../components/Sentence";

const App: React.FC = () => {
  return (
    <>
      <Head>
        <title>CCG Playground</title>
        <link rel="icon" href="/favicon.ico" />
      </Head>
      <GridLayout className="layout" cols={6} rowHeight={100} width={1800}>
        <div key="sentence-form" data-grid={{ x: 0, y: 0, w: 3, h: 4 }}>
          <Sentence />
        </div>
        <div key="constituent-tree-viewer" data-grid={{ x: 0, y: 4, w: 3, h: 4 }}>
          <ConstituentTree />
        </div>
        <div key="grammar-viewer" data-grid={{ x: 3, y: 0, w: 3, h: 4 }}>
          <Grammar />
        </div>
        <div key="ccg-tree-viewer" data-grid={{ x: 3, y: 4, w: 3, h: 4 }}>
          <Derivation />
        </div>
      </GridLayout>
    </>
  );
};

export default App;
