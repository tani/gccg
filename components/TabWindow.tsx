import React, { useContext, useState } from "react";

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
