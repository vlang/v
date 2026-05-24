import { performance } from "perf_hooks";
import { Entity, Column, PrimaryGeneratedColumn, DataSource } from "typeorm";

type Response = {
  insert: number[];
  select: number[];
  update: number[];
};

@Entity("benchmark")
export class Task {
  @PrimaryGeneratedColumn()
  id?: number;

  @Column("text")
  title!: string;

  @Column("text")
  status!: string;
}

export const appDataSource = new DataSource({
  type: "sqlite",
  database: ":memory:",
  dropSchema: true,
  entities: [Task],
  synchronize: true, // create a new table
  logging: false,
});

export async function sqlite_memory(count: Number): Promise<Response> {
  var insert_stopwatchs: number[] = [];
  let select_stopwatchs: number[] = [];
  let update_stopwatchs: number[] = [];

  let sw = performance;

  const taskRepository = appDataSource.getRepository(Task);

  // inserts
  for (let index = 0; index < count; index++) {
    const task_model = new Task();
    task_model.title = "a";
    task_model.status = "done";

    const start = sw.now();
    await taskRepository.save(task_model).then((value) => {
      const insert_stopwatch = (sw.now() - start) * 1000000;
      insert_stopwatchs.push(Math.floor(insert_stopwatch)); //nanoseconds
    });
  }

  // selects
  for (let index = 0; index < count; index++) {
    const start = sw.now();
    await taskRepository.find().then((value) => {
      const select_stopwatch = (sw.now() - start) * 1000000;
      select_stopwatchs.push(Math.floor(select_stopwatch)); //nanoseconds
    });
  }

  // updates
  for (let index = 0; index < count; index++) {
    const taskToUpdate = await taskRepository.findOneBy({ id: 1 });

    taskToUpdate!.title = "b";
    taskToUpdate!.status = "finish";

    const start = sw.now();
    await taskRepository.save(taskToUpdate!).then((value) => {
      const update_stopwatch = (sw.now() - start) * 1000000;
      update_stopwatchs.push(Math.floor(update_stopwatch)); //nanoseconds
    });
  }

  // taskRepository.find().then((value) => {
  //   console.log(`value: %j`, value);
  // });

  let response: Response = {
    insert: insert_stopwatchs,
    select: select_stopwatchs,
    update: update_stopwatchs,
  };

  return Promise.resolve(response);
}

// export function sqlite_file(count: Number): Response {
//   var insert_stopwatchs: number[] = [];
//   let select_stopwatchs: number[] = [];
//   let update_stopwatchs: number[] = [];

//   for (let index = 0; index < count; index++) {
//     insert_stopwatchs.push(index);
//   }

//   let response: Response = {
//     insert: insert_stopwatchs,
//     select: select_stopwatchs,
//     update: update_stopwatchs,
//   };

//   return response;
// }

// export function postgres(count: Number): Response {
//   var insert_stopwatchs: number[] = [];
//   let select_stopwatchs: number[] = [];
//   let update_stopwatchs: number[] = [];

//   for (let index = 0; index < count; index++) {
//     insert_stopwatchs.push(index);
//   }

//   let response: Response = {
//     insert: insert_stopwatchs,
//     select: select_stopwatchs,
//     update: update_stopwatchs,
//   };

//   return response;
// }

// export function mysql(count: Number): Response {
//   var insert_stopwatchs: number[] = [];
//   let select_stopwatchs: number[] = [];
//   let update_stopwatchs: number[] = [];

//   for (let index = 0; index < count; index++) {
//     insert_stopwatchs.push(index);
//   }

//   let response: Response = {
//     insert: insert_stopwatchs,
//     select: select_stopwatchs,
//     update: update_stopwatchs,
//   };

//   return response;
// }
