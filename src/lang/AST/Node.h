#pragma once

enum NodeType {
    DecimalConstant,
    DoubleConstant,
    BinaryOperation,
    UnaryOperation,
    Variable,
    FunctionCall
};

typedef struct node {
    enum NodeType type;
} Node;