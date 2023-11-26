export default class Player {
  pieces = [];
  color = "";

  constructor(color, pieces) {
    this.color = color;
    this.pieces = pieces;
  }

  getPieces = () => {
    return this.pieces;
  };
}
