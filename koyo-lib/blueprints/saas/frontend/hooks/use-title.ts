import * as React from "react";

export const useTitle = (subtitle?: string) => {
  React.useEffect(() => {
    let title = "ExampleApp";
    if (subtitle !== undefined) {
      title = `${subtitle} – ${title}`;
    }
    document.title = title;
  });
};
