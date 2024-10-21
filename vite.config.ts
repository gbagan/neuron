import { defineConfig } from 'vite';
import civet from '@danielx/civet/vite';
import solidPlugin from 'vite-plugin-solid';

export default defineConfig({
  base: "./",
  plugins: [
    civet({ ts: "preserve" }),
    solidPlugin(),
  ],
  build: {
    target: 'esnext',
  },
  server: {
    port: 3000,
  },
});
