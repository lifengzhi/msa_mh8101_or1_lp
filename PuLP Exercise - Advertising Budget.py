# To add a new cell, type '#%%'
# To add a new markdown cell, type '#%% [markdown]'
#%% [markdown]
# # Advertising Budget
# 
# In this notebook, we will have a simple walkthrough on using PuLP to solve a simple optimization problem.
# 
# If you need help on using Jupyter notebooks, click <a href='#help'>here</a>. 
#%% [markdown]
# ---
# 
# ## Installing PuLP
# 
# We need to install the PuLP library. To do so go to the command line (for Windows OS) or open a Terminal window (Mac OS / Linux OS) and type in command: 
# 
# ```
# pip install pulp
# ```

#%%
# To run this cell, hit "SHIFT + ENTER"
# If an error message appears, follow the above instructions to install PuLP.

import pulp

print("PuLP library imported.")

#%% [markdown]
# ## Your Task
# 
# In this notebook, I will provide the code for the linear programme solving the *Brewer's Problem*. Specifically, we solve the program:
# 
# $$
# \begin{array}{crcrl}
# \max & 13A & + & 23B\\ 
# \text{subject to} 
# &5A &+ &15B &\le 480\\ 
# &4A &+ &4B  &\le 160\\ 
# &35A& + &20B &\le 1190\\ 
# &&& A &\ge 0\\
# &&& B &\ge 0\\
# \end{array}
# $$
# 
# **Your task**: write the code for *Advertising Budget Problem*. In other words, you will implement the code to solve:
# 
# 
# $$ 
# \begin{array}{crcrl}
# \max & 50x_1 & + & 100x_2\\ 
# \text{subject to} 
# &7x_1 &+ &2x_2 &\ge 28\\ 
# &2x_1 &+ &12x_2  &\ge 24\\ 
# &&& x_1 &\ge 0\\
# &&& x_2 &\ge 0\\
# \end{array}
# $$
#%% [markdown]
# ## (I) Instantiate Our Problem Class
# 
# For the *Brewer's Problem*, we instantiate the problem class with the following:
# <span style="color:blue">
# ```python
# model = pulp.LpProblem("Brewer's Problem", pulp.LpMaximize)
# 
# A = pulp.LpVariable('Ale', lowBound=0)
# B = pulp.LpVariable('Beer', lowBound=0)
# ```
# </span>


#%%
# Instantiate the problem class for the Advertising Budget Problem


#%% [markdown]
# ## (II) Add the Objective Function to the Model
# 
# For the *Brewer's Problem*, we add the objective function with the following:
# 
# ```python
# model += 13 * A + 23 * B, "Profit"
# ```
# 

#%%
# Add the Objective function for the Advertising Budget Problem

#%% [markdown]
# ## (III) Add the Constraints to the Model
# 
# For the *Brewer's Problem*, we add the constraints with the following:
# 
# ```python
# model += 5*A  + 15*B <= 480, "corn" 
# model += 4*A  + 4*B  <= 160, "hops" 
# model += 35*A + 20*B <= 1190, "malt"
# ```
# 

#%%
# Add the constraints for the Advertising Budget Problem

#%% [markdown]
# ## (IV) Display the model 
# 
# To display the model, type
# ```python
# print(model)
# ```

#%%
# Display the model for the Advertising Budget Problem

#%% [markdown]
# ## (V) Solve the Model
# 
# To solve the model, type
# ```python
# model.solve()
# print("model status: ",pulp.LpStatus[model.status])
# ```

#%%
# Solve the Advertising Budget Problem
# We will learn more about the meanings of the various model status in the lecture.

#%% [markdown]
# ## (VII) Display the Optimal Solution
# 
# For the *Brewer's Problem*, we type
# ```python
# print("Number of Ale = {}".format(A.varValue))
# print("Number of Beer = {}".format(B.varValue))
# ```

#%%
# Display the optimal solution for the Advertising Budget Problem

#%% [markdown]
# There are **more functionalities** in the PuLP library. You can read the full documentation here (https://pythonhosted.org/PuLP/).
#%% [markdown]
# ---
# 
# ---
#%% [markdown]
# <a id='help'></a>
# **Using Jupyter Notebooks**. When you click to the left of this box, you will notice that this box is highlighted by a slighly larger box. This is a *cell*. 
# 
# There are three types of cells in a notebook.
# 
# 1. Markdown.
# 2. Code.
# 3. Raw.
# 
# You can change the type of cell by going to the tool bar.
# 
# You can *evaluate* cells by hitting **Shift+Enter**. Depending on the type of cells, you will have different outputs.
#%% [markdown]
# ---
# 
# This is a **markdown** cell. Markdown is a lightweight markup language is similar to *html* with significantly less functionalities. However, the syntax is much simpler. You can find a [Markdown Cheatsheet here](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet).
# 
# ---

#%%
# This is a CODE cell.
# After you hit Shift+Enter, it evaluates the cell in Python.
# Take note that in Python, to comment lines, you use the symbol #


print("Hello World!")

----

This is a *raw* cell. Nothing happens when you hit Shift+Enter.

----#%% [markdown]
# **Answering Questions**. You may choose to use *raw* or *markdown* cells to answer the questions. Of course, if the answer requires you to run a routine in Python, please use a *code* cell.
# 

#%%
# Code cell for Brewer's Problem.

import pulp

modelBrewer = pulp.LpProblem("Brewer's Problem", pulp.LpMaximize)

dvA = pulp.LpVariable('Ale', lowBound=0)
dvB = pulp.LpVariable('Beer', lowBound=0)

modelBrewer += 13 * dvA + 23 * dvB, "Profit"

modelBrewer += 5 * dvA + 15 * dvB <= 480, "corn"
modelBrewer += 4 * dvA + 4 * dvB <= 160, "hops"
modelBrewer += 35 * dvA + 20 * dvB <= 1190, "malt"

print(modelBrewer)

modelBrewer.solve()
print("model status: ",pulp.LpStatus[modelBrewer.status])

print("Number of Ale = {}".format(dvA.varValue))
print("Number of Beer = {}".format(dvB.varValue))

#%%
# Code cell for Advertising Budget.
import pulp

budgetModel = pulp.LpProblem("Advertising Budget", pulp.pulp.LpMinimize)

dvComedy = pulp.LpVariable('Comedy', lowBound=0)
dvFootball = pulp.LpVariable('Football', lowBound=0)

budgetModel += 50 * dvComedy + 100 * dvFootball, "Cost"

budgetModel += 7 * dvComedy + 2 * dvFootball >= 28, "women"
budgetModel += 2 * dvComedy + 12 * dvFootball >= 24, "men"

print(budgetModel)

budgetModel.solve()
print("model status: ",pulp.LpStatus[budgetModel.status])

print("Number of Comedy = {}".format(dvComedy.varValue))
print("Number of Football = {}".format(dvFootball.varValue))