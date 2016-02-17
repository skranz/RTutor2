# Tasks:
#
# - Examples for tasks are quizzes and chunks that an user needs to solve.
#
# - A task is an object that requires a user input.
#
# - In general, tasks can be solved, and scores, points and awards can be given
#   max.points are assigned to a task.
#
# - A task can internally contain subtasks,
#   but from RTutor perspective, a task is an atom.
#
# - Each task can be mapped to a block or chunk in the solution file,
#   but several subblocks of that block or chunk can describe the task.
#
# - Each task should have a unique id, if not provided in the rmd, we should create one
#
# - Tasks may or may not change the stud.env. This depends on task type and options.
#
# - Stored user data for task:
#
#     - Minimum data storage for a task is solved, points, and score.
#       This is stored in ups. We may display precompiled task.ui
#       solely depending on solved or not, e.g. with preknitted chunks.
#     - Additionally tasks can have a task state ts that contains additional
#       information. The structure of the task state depends on the task object.
#       For a chunk, the task state could contain the user code and information
#       about which tests are passed.
#     - Tasks that can change stud.env have a post task environment.
#     - Task states and environments may be stored in ups
#       or in separate files or they may not be stored.
#
# - Atomizing: It should be possible to present tasks separately to users,
#   independently of the problem set context. Depending on the task, we may
#   have to provide a pretask environment, however.
#   For example, in a lecture, we may push a quiz task from RTutor frames,
#   as a live "clicker" quiz to students' mobile phones.
