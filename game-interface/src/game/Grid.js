import React from 'react';
import './../static/styles/grid.css';

// Component that represents a single grid on the board
let Grid = ({ color, handleClick, row, column, translucent, powerupText }) => {
  return (
    <div
      className='grid'
      style={{
        backgroundColor: color,
        width: '25px',
        height: '25px',
        opacity: translucent ? 0.25 : 1,
        fontSize: '10px',
        textAlign: 'center',
        paddingTop: 'auto',
        paddingBottom: 'auto',
      }}
      onClick={() => handleClick(row, column)}
    >
      {powerupText}
    </div>
  );
};

export default Grid;
