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

import io.vertx.codegen.annotations.ProxyGen;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;

/**
 * The services to interact with the {@link ServiceApiSimulatorService}.
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ProxyGen
public interface ServiceApiSimulatorService {

	/**
	 * The address of this service.
	 */
	String SIMULATOR_ADDRESS = "wenet_common.service.ServiceApiSimulator";

	/**
	 * Create a proxy of the {@link WeNetServiceApiService}.
	 *
	 * @param vertx where the service has to be used.
	 *
	 * @return the task.
	 */
	static ServiceApiSimulatorService createProxy(Vertx vertx) {

		return new ServiceApiSimulatorServiceVertxEBProxy(vertx, ServiceApiSimulatorServiceImpl.SIMULATOR_ADDRESS);
	}

	/**
	 * Return an application.
	 *
	 * @param id              identifier of the app to get.
	 * @param retrieveHandler handler to manage the retrieve process.
	 */
	void retrieveApp(String id, Handler<AsyncResult<JsonObject>> retrieveHandler);

	/**
	 * Defined method only for testing and can store an APP.
	 *
	 * @param app           to create.
	 * @param createHandler handler to manage the creation process.
	 */
	void createApp(JsonObject app, Handler<AsyncResult<JsonObject>> createHandler);

	/**
	 * Defined method only for testing and can delete an APP.
	 *
	 * @param id            identifier of the application to remove.
	 * @param deleteHandler handler to manage the delete process.
	 */
	void deleteApp(String id, Handler<AsyncResult<Void>> deleteHandler);

}
