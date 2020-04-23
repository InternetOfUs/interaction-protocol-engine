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

package eu.internetofus.common;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;

/**
 * Test the {@link ServiceApiSimulatorService}
 *
 * @see ServiceApiSimulatorService
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ExtendWith(VertxExtension.class)
public class ServiceApiSimulatorServiceTest {

	/**
	 * The context that can be used to create the clients.
	 */
	private static WeNetModuleContext context;

	/**
	 * Create the context to use.
	 */
	@BeforeAll
	public static void startServiceApiSimulator() {

		final int serviceApiPort = Containers.nextFreePort();
		Containers.createAndStartServiceApiSimulator(serviceApiPort);
		final JsonObject configuration = new JsonObject().put("wenetComponents", new JsonObject().put("service",
				new JsonObject().put("host", "localhost").put("port", serviceApiPort).put("apiPath", "")));
		context = new WeNetModuleContext(null, configuration);
	}

	/**
	 * Should create, retrieve and delete an application.
	 *
	 * @param vertx       that contains the event bus to use.
	 * @param testContext context over the tests.
	 */
	@Test
	public void shouldCreateRetrieveAndDeleteApp(Vertx vertx, VertxTestContext testContext) {

		context.vertx = vertx;
		final ServiceApiSimulatorService service = ServiceApiSimulatorService.create(context);
		service.createApp(new JsonObject(), testContext.succeeding(create -> {

			final String id = create.getString("appId");
			service.retrieveApp(id, testContext.succeeding(retrieve -> testContext.verify(() -> {

				assertThat(create).isEqualTo(retrieve);
				service.deleteApp(id, testContext.succeeding(empty -> {

					service.retrieveApp(id, testContext.failing(handler -> {
						testContext.completeNow();

					}));

				}));

			})));

		}));

	}

}
