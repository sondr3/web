FROM node:15

WORKDIR /usr/src/web

COPY package.json yarn.lock tsconfig.json ./
RUN yarn install --frozen-lockfile --production

COPY . .

RUN yarn compile
RUN yarn build