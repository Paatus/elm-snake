import { defineConfig } from 'vite'
import elmPlugin from 'vite-plugin-elm'
import tailwindcss from '@tailwindcss/vite'

export default defineConfig({
  base: process.env.DEV ? '' : '/elm-snake/',
  plugins: [elmPlugin(), tailwindcss()],
  build: { outDir: "docs" }
})
