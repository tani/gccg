import React, { useState } from "react";

const TabContext = React.createContext({
  activeTab: "",
  toggleTab: (_: string) => {
    return;
  },
});

export const TabPanel: React.FC<{ title: string; [x: string]: unknown }> = (
  props
) => {
  return (
    <TabContext.Consumer>
      {(context) =>
          <div {...props} hidden={context.activeTab !== props.title}></div>
      }
    </TabContext.Consumer>
  );
};

export const TabList: React.FC<{ [x: string]: unknown }> = (props) => {
  return <menu {...props} />;
};

export const Tab: React.FC<{ target: string; [x: string]: unknown }> = (
  props
) => {
  return (
    <TabContext.Consumer>
      {(context) => (
        <button
          {...props}
          aria-selected={context.activeTab === props.target}
          onClick={() => context.toggleTab(props.target)}
        />
      )}
    </TabContext.Consumer>
  );
};

export const Tabs: React.FC<{ defaulTarget: string; [x: string]: unknown }> = (
  props
) => {
  const [activeTab, toggleTab] = useState(props.defaulTarget);
  return (
    <TabContext.Provider value={{ activeTab, toggleTab }}>
      <div {...props} />
    </TabContext.Provider>
  );
};
