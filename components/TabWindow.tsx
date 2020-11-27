import React, { useContext, useState } from "react";
import JSONTree from "react-json-tree";
import theme from "../lib/theme";

const TabContext = React.createContext({
  activeTab: "",
  toggleTab: (_: string) => {
    return;
  },
});

export const TabPanel: React.FC<{ title: string } & JSX.IntrinsicElements["div"]> = (props) => {
  const context = useContext(TabContext);
  const { title, ...options } = props;
  return <div {...options} hidden={context.activeTab !== title}></div>;
};

export const TabList: React.FC<JSX.IntrinsicElements["div"]> = (props) => {
  return <menu {...props} />;
};

export const Tab: React.FC<{ target: string } & JSX.IntrinsicElements["button"]> = (props) => {
  const context = useContext(TabContext);
  const { target, ...options } = props;
  return <button {...options} aria-selected={context.activeTab === target} onClick={() => context.toggleTab(target)} />;
};

export const Tabs: React.FC<{ defaulTarget: string } & JSX.IntrinsicElements["div"]> = (props) => {
  const { defaulTarget, ...options } = props;
  const [activeTab, toggleTab] = useState(defaulTarget);
  return (
    <TabContext.Provider value={{ activeTab, toggleTab }}>
      <div {...options} />
    </TabContext.Provider>
  );
};

export const TabWindow: React.FC<{ title: string; data: unknown }> = (props) => {
  const [state, setState] = useState({ fontSize: "medium" });
  const style: React.CSSProperties = {
    fontSize: state.fontSize,
  };
  const fontSizes = ["xx-small", "x-small", "small", "medium", "large", "x-large", "xx-large"];
  return (
    <div className="window">
      <div className="title-bar">
        <div className="titleb-bar-text">{props.title}</div>
      </div>
      <Tabs defaulTarget="main" style={style} className="window-body">
        <TabList role="tablist">
          <Tab role="tab" target="main">
            Visualization
          </Tab>
          <Tab role="tab" target="raw">
            Raw
          </Tab>
          <Tab role="tab" target="configuration">
            Configuration
          </Tab>
        </TabList>
        <TabPanel role="tabpanel" title="main">{props.children}</TabPanel>
        <TabPanel role="tabpanel" title="raw">
          <JSONTree data={props.data} theme={theme} />
        </TabPanel>
        <TabPanel role="tabpanel" title="configuration">
          <label htmlFor="fontsize">Font size</label>
          <select name="fontsize" onChange={(e) => setState({ fontSize: e.target.value })}>
            {fontSizes.map((fontSize) => (
              <option key={fontSize} value={fontSize} selected={state.fontSize === fontSize}>
                {fontSize}
              </option>
            ))}
          </select>
        </TabPanel>
      </Tabs>
    </div>
  );
};
