import './style.css'
import { Elm } from './Main.elm';

const HS_KEY = "elm-snake-highscore";

const app = Elm.Main.init({
  node: document.getElementById('app'),
  flags: { highScore: parseInt(window.localStorage.getItem(HS_KEY) || '') || 0 }
});

app.ports.setHighscore.subscribe((score: number) => {
  window.localStorage.setItem(HS_KEY, score.toString());
});
