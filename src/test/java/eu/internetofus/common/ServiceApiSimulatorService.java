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

import eu.internetofus.common.services.WeNetServiceApiServiceImpl;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.client.WebClient;

/**
 * Service used to interact with the {@link ServiceApiSimulator}.
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class ServiceApiSimulatorService extends WeNetServiceApiServiceImpl {

	/**
	 * Create a new service to interact with the WeNet interaction protocol engine.
	 *
	 * @param client to interact with the other modules.
	 * @param conf   configuration.
	 */
	public ServiceApiSimulatorService(WebClient client, JsonObject conf) {

		super(client, conf);

	}

	/**
	 * Call the {@link ServiceApiSimulator} to create an application.
	 *
	 * {@inheritDoc}
	 */
	@Override
	public void createApp(JsonObject app, Handler<AsyncResult<JsonObject>> createHandler) {

		this.post("/app", app, createHandler);

	}

	/**
	 * Call the {@link ServiceApiSimulator} to delete an application.
	 *
	 * {@inheritDoc}
	 */
	@Override
	public void deleteApp(String id, Handler<AsyncResult<Void>> deleteHandler) {

		this.delete("/app/" + id, deleteHandler);

	}

	/**
	 * Create a service that will link to the simulator service.
	 *
	 * @param context used to create the service.
	 *
	 * @return the created service.
	 */
	public static ServiceApiSimulatorService create(WeNetModuleContext context) {

		final WebClient client = WebClient.create(context.vertx);
		final JsonObject conf = context.configuration.getJsonObject("wenetComponents", new JsonObject())
				.getJsonObject("service", new JsonObject());
		return new ServiceApiSimulatorService(client, conf);
	}

}
