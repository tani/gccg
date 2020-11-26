import { browserAdaptor } from "mathjax-full/js/adaptors/browserAdaptor";
import { RegisterHTMLHandler } from "mathjax-full/js/handlers/html";
import { TeX } from "mathjax-full/js/input/tex";
import { AllPackages } from "mathjax-full/js/input/tex/AllPackages";
import { mathjax } from "mathjax-full/js/mathjax";
import { CHTML } from "mathjax-full/js/output/chtml";
import { OptionList } from "mathjax-full/js/util/Options";
import Head from "next/head";
import React, { memo, useEffect, useState } from "react";
const adaptor = browserAdaptor();
RegisterHTMLHandler(adaptor);
const tex = new TeX({ packages: AllPackages });
const chtml = new CHTML<HTMLElement, unknown, unknown>({
  fontURL:
    "https://cdn.jsdelivr.net/npm/mathjax@3/es5/output/chtml/fonts/woff-v2",
});
const mathDocument = mathjax.document("", { InputJax: tex, OutputJax: chtml });

const MathJax: React.FC<{ src: string; options: OptionList }> = (props) => {
  const [stylesheet, setState] = useState("");
  useEffect(() => {
    setState(adaptor.textContent(chtml.styleSheet(mathDocument)));
  }, []);
  const __html = adaptor.outerHTML(mathDocument.convert(props.src, props.options))
  return (
    <div>
      <Head>
        <style key="mathjax-stylesheet">{stylesheet}</style>
      </Head>
      <div dangerouslySetInnerHTML={{__html }}/>
    </div>
  );
};

export default memo(MathJax);
