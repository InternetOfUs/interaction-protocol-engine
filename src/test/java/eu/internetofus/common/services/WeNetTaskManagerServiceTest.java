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

package eu.internetofus.common.services;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import eu.internetofus.wenet_interaction_protocol_engine.WeNetInteractionProtocolEngineIntegrationExtension;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxTestContext;

/**
 * Test the {@link WeNetTaskManagerService}.
 *
 * @see WeNetTaskManagerService
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ExtendWith(WeNetInteractionProtocolEngineIntegrationExtension.class)
public class WeNetTaskManagerServiceTest {

	/**
	 * Should not create a bad task.
	 *
	 * @param service     to check that it can not create the task.
	 * @param testContext context over the tests.
	 */
	@Test
	public void shouldNotCreateBadTask(WeNetTaskManagerService service, VertxTestContext testContext) {

		service.createTask(new JsonObject().put("undefinedField", "value"), testContext.failing(handler -> {
			testContext.completeNow();

		}));

	}

	/**
	 * Should not retrieve undefined task.
	 *
	 * @param service     to check that it can not create the task.
	 * @param testContext context over the tests.
	 */
	@Test
	public void shouldNotRretrieveUndefinedTask(WeNetTaskManagerService service, VertxTestContext testContext) {

		service.retrieveTask("undefined-task-identifier", testContext.failing(handler -> {
			testContext.completeNow();

		}));

	}

	/**
	 * Should not delete undefined task.
	 *
	 * @param service     to check that it can not create the task.
	 * @param testContext context over the tests.
	 */
	@Test
	public void shouldNotDeleteUndefinedTask(WeNetTaskManagerService service, VertxTestContext testContext) {

		service.deleteTask("undefined-task-identifier", testContext.failing(handler -> {
			testContext.completeNow();

		}));

	}

	/**
	 * Should retrieve created task.
	 *
	 * @param service     to check that it can not create the task.
	 * @param testContext context over the tests.
	 */
	@Test
	public void shouldRetrieveCreatedTask(WeNetTaskManagerService service, VertxTestContext testContext) {

		service.createTask(new JsonObject(), testContext.succeeding(create -> {

			final String id = create.getString("taskId");
			service.retrieveTask(id, testContext.succeeding(retrieve -> testContext.verify(() -> {

				assertThat(create).isEqualTo(retrieve);
				service.deleteTask(id, testContext.succeeding(empty -> {

					service.retrieveTask(id, testContext.failing(handler -> {
						testContext.completeNow();

					}));

				}));

			})));

		}));

	}

}
