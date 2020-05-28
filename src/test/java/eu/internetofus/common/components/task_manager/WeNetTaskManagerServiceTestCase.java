/*
 * -----------------------------------------------------------------------------
 *
 * Copyright (c) 2019 - 2022 UDT-IA, IIIA-CSIC
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * -----------------------------------------------------------------------------
 */

package eu.internetofus.common.components.task_manager;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxTestContext;

/**
 * Test the {@link WeNetTaskManagerService}.
 *
 * @see WeNetTaskManagerService
 *
 * @author UDT-IA, IIIA-CSIC
 */
public abstract class WeNetTaskManagerServiceTestCase {

	/**
	 * Should not create a bad task.
	 *
	 * @param vertx       that contains the event bus to use.
	 * @param testContext context over the tests.
	 */
	@Test
	public void shouldNotCreateBadTask(Vertx vertx, VertxTestContext testContext) {

		WeNetTaskManagerService.createProxy(vertx).createTask(new JsonObject().put("undefinedField", "value"),
				testContext.failing(handler -> {
					testContext.completeNow();
				}));

	}

	/**
	 * Should not retrieve undefined task.
	 *
	 * @param vertx       that contains the event bus to use.
	 * @param testContext context over the tests.
	 */
	@Test
	public void shouldNotRretrieveUndefinedTask(Vertx vertx, VertxTestContext testContext) {

		WeNetTaskManagerService.createProxy(vertx).retrieveTask("undefined-task-identifier",
				testContext.failing(handler -> {
					testContext.completeNow();
				}));

	}

	/**
	 * Should not delete undefined task.
	 *
	 * @param vertx       that contains the event bus to use.
	 * @param testContext context over the tests.
	 */
	@Test
	public void shouldNotDeleteUndefinedTask(Vertx vertx, VertxTestContext testContext) {

		WeNetTaskManagerService.createProxy(vertx).deleteTask("undefined-task-identifier", testContext.failing(handler -> {
			testContext.completeNow();

		}));

	}

	/**
	 * Should create, retrieve and delete a task.
	 *
	 * @param vertx       that contains the event bus to use.
	 * @param testContext context over the tests.
	 */
	@Test
	public void shouldCreateRetrieveAndDeleteTask(Vertx vertx, VertxTestContext testContext) {

		new TaskTest().createModelExample(1, vertx, testContext, testContext.succeeding(task -> {
			final WeNetTaskManagerService service = WeNetTaskManagerService.createProxy(vertx);
			service.createTask(task, testContext.succeeding(create -> {

				final String id = create.id;
				service.retrieveTask(id, testContext.succeeding(retrieve -> testContext.verify(() -> {

					assertThat(create).isEqualTo(retrieve);
					service.deleteTask(id, testContext.succeeding(empty -> {

						service.retrieveTask(id, testContext.failing(handler -> {
							testContext.completeNow();

						}));

					}));

				})));

			}));
		}));
	}

	/**
	 * Should not create a bad task type.
	 *
	 * @param vertx       that contains the event bus to use.
	 * @param testContext context over the tests.
	 */
	@Test
	public void shouldNotCreateBadTaskType(Vertx vertx, VertxTestContext testContext) {

		WeNetTaskManagerService.createProxy(vertx).createTaskType(new JsonObject().put("undefinedField", "value"),
				testContext.failing(handler -> {
					testContext.completeNow();

				}));

	}

	/**
	 * Should not retrieve undefined task type.
	 *
	 * @param vertx       that contains the event bus to use.
	 * @param testContext context over the tests.
	 */
	@Test
	public void shouldNotRretrieveUndefinedTaskType(Vertx vertx, VertxTestContext testContext) {

		WeNetTaskManagerService.createProxy(vertx).retrieveTaskType("undefined-task-type-identifier",
				testContext.failing(handler -> {
					testContext.completeNow();

				}));

	}

	/**
	 * Should not delete undefined task type.
	 *
	 * @param vertx       that contains the event bus to use.
	 * @param testContext context over the tests.
	 */
	@Test
	public void shouldNotDeleteUndefinedTaskType(Vertx vertx, VertxTestContext testContext) {

		WeNetTaskManagerService.createProxy(vertx).deleteTaskType("undefined-task-type-identifier",
				testContext.failing(handler -> {
					testContext.completeNow();

				}));

	}

	/**
	 * Should create, retrieve and delete a task type.
	 *
	 * @param vertx       that contains the event bus to use.
	 * @param testContext context over the tests.
	 */
	@Test
	public void shouldCreateRetrieveAndDeleteTaskType(Vertx vertx, VertxTestContext testContext) {

		final TaskType taskType = new TaskTypeTest().createModelExample(1);
		final WeNetTaskManagerService service = WeNetTaskManagerService.createProxy(vertx);
		service.createTaskType(taskType, testContext.succeeding(create -> {

			final String id = create.id;
			service.retrieveTaskType(id, testContext.succeeding(retrieve -> testContext.verify(() -> {

				assertThat(create).isEqualTo(retrieve);
				service.deleteTaskType(id, testContext.succeeding(empty -> {

					service.retrieveTaskType(id, testContext.failing(handler -> {
						testContext.completeNow();

					}));

				}));

			})));

		}));

	}

}