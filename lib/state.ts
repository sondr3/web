interface State {
  styles: Map<string, string>;
  pages: Map<string, string>;
}

const initialState: State = {
  styles: new Map<string, string>(),
  pages: new Map<string, string>(),
};

export const siteState = initialState;
