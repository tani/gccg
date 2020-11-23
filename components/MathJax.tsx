import React from "react";
import { mathjax } from "mathjax-full/js/mathjax";
import { TeX } from "mathjax-full/js/input/tex";
import { SVG } from "mathjax-full/js/output/svg";
import { liteAdaptor } from "mathjax-full/js/adaptors/liteAdaptor";
import { RegisterHTMLHandler } from "mathjax-full/js/handlers/html";
import { AllPackages } from "mathjax-full/js/input/tex/AllPackages";
import { OptionList } from "mathjax-full/js/util/Options";
const adaptor = liteAdaptor();
RegisterHTMLHandler(adaptor);
const tex = new TeX({ packages: AllPackages });
const svg = new SVG({ fontCache: "none" });
const mathDocument = mathjax.document("", { InputJax: tex, OutputJax: svg });

const MathJax: React.FC<{ src: string; options: OptionList }> = (props) => {
  return (
    <div
      dangerouslySetInnerHTML={{
        __html: adaptor.outerHTML(
          mathDocument.convert(props.src, props.options)
        ),
      }}
    />
  );
};

export default MathJax;
