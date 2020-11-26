import Head from "next/head";
import React from "react";
import { Responsive, WidthProvider} from "react-grid-layout";
import ConstituentTree from "../components/ConstituentTree";
import Derivation from "../components/Derivation";
import Grammar from "../components/Grammar";
import Sentence from "../components/Sentence";

const ResponsiveGridLayout = WidthProvider(Responsive)

const App: React.FC = () => {
  return (
    <>
      <Head>
        <title>CCG Playground</title>
        <link rel="icon" href="/favicon.ico" />
      </Head>
      <ResponsiveGridLayout className="layout" rowHeight={100} cols={{lg: 12, md: 12, sm: 12, xs: 6, xss: 6}}>
        <div key="sentence-form" data-grid={{ x: 0, y: 0, w: 6, h: 4 }}>
          <Sentence />
        </div>
        <div key="constituent-tree-viewer" data-grid={{ x: 0, y: 4, w: 6, h: 4 }}>
          <ConstituentTree />
        </div>
        <div key="grammar-viewer" data-grid={{ x: 6, y: 0, w: 6, h: 4 }}>
          <Grammar />
        </div>
        <div key="ccg-tree-viewer" data-grid={{ x: 6, y: 4, w: 6, h: 4 }}>
          <Derivation />
        </div>
      </ResponsiveGridLayout>
    </>
  );
};

export default App;
