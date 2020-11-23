import React from "react";
import { mathjax } from "mathjax-full/js/mathjax";
import { TeX } from "mathjax-full/js/input/tex";
import { CHTML } from "mathjax-full/js/output/chtml";
import { liteAdaptor } from "mathjax-full/js/adaptors/liteAdaptor";
import { RegisterHTMLHandler } from "mathjax-full/js/handlers/html";
import { AllPackages } from "mathjax-full/js/input/tex/AllPackages";
import { OptionList } from "mathjax-full/js/util/Options";
import Head from "next/head";
const adaptor = liteAdaptor();
RegisterHTMLHandler(adaptor);
const tex = new TeX({ packages: AllPackages });
const chtml = new CHTML({
  fontURL:
    "https://cdn.jsdelivr.net/npm/mathjax@3/components/output/chtml/fonts/woff-v2",
});
const mathDocument = mathjax.document("", { InputJax: tex, OutputJax: chtml });

const MathJax: React.FC<{ src: string; options: OptionList }> = (props) => {
  const stylesheet = adaptor.textContent(chtml.styleSheet(mathDocument) as any);
  return (
    <div>
      <Head>
        <style key="mathjax-stylesheet">{stylesheet}</style>
      </Head>
      <div
        dangerouslySetInnerHTML={{
          __html: adaptor.outerHTML(
            mathDocument.convert(props.src, props.options)
          ),
        }}
      />
    </div>
  );
};

export default MathJax;
