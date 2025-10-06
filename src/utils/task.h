#pragma once

using fSubmapTask = std::function<void()>;

class CSubmapTask : public QRunnable {
 public:
  CSubmapTask(fSubmapTask task) : task(task) {}
  ~CSubmapTask() = default;

  void run() { task(); }

 private:
  fSubmapTask task;
};
