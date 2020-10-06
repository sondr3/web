declare namespace NodeJS {
  interface Global {
    production: boolean;
    verbosity: number;
  }
  interface ProcessEnv {
    production?: string;
    verbosity?: string;
  }
}
