import React from 'react';

const Piece = ({ piece, color, handleSelectPiece, currentTurn }) => {
  const gridSize = '15px';
  let pieceSize = piece.length * 15 + 'px';
  return (
    <div
      className='piece'
      style={{ height: pieceSize, width: pieceSize, margin: '2%' }}
      onClick={() => {
        if (color === currentTurn) handleSelectPiece(piece);
      }}
    >
      {piece.map((row) => {
        return row.map((grid) => {
          return (
            <div
              style={{
                height: gridSize,
                width: gridSize,
                backgroundColor: grid === 0 ? 'white' : color,
                margin: 0,
                opacity: currentTurn === color ? 1 : 0.6,
              }}
            ></div>
          );
        });
      })}
    </div>
  );
};

export default Piece;
