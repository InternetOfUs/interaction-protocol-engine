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
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
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

package eu.internetofus.wenet_interaction_protocol_engine.persistence;

import eu.internetofus.common.model.Model;
import eu.internetofus.common.components.interaction_protocol_engine.State;
import io.vertx.codegen.annotations.GenIgnore;
import io.vertx.codegen.annotations.ProxyGen;
import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Promise;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.mongo.MongoClient;
import io.vertx.serviceproxy.ServiceBinder;

/**
 * The service to manage the {@link State} on the database.
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ProxyGen
public interface StatesRepository {

  /**
   * The address of this service.
   */
  String ADDRESS = "wenet_interaction_protocol_engine.persistence.states";

  /**
   * Register this service.
   *
   * @param vertx   that contains the event bus to use.
   * @param pool    to create the database connections.
   * @param version of the schemas.
   *
   * @return the future that inform when the repository will be registered or not.
   */
  static Future<Void> register(final Vertx vertx, final MongoClient pool, final String version) {

    final var repository = new StatesRepositoryImpl(pool, version);
    new ServiceBinder(vertx).setAddress(StatesRepository.ADDRESS).register(StatesRepository.class, repository);
    return repository.migrateDocumentsToCurrentVersions();

  }

  /**
   * Create a proxy of the {@link StatesRepository}.
   *
   * @param vertx where the service has to be used.
   *
   * @return the state.
   */
  static StatesRepository createProxy(final Vertx vertx) {

    return new StatesRepositoryVertxEBProxy(vertx, StatesRepository.ADDRESS);
  }

  /**
   * Search for the state with the specified attributes.
   *
   * @param communityId identifier of the community in the state.
   * @param taskId      identifier of the task in the state.
   * @param userId      identifier of the user in the state.
   * @return the future search model.
   */
  @GenIgnore
  default Future<State> searchState(final String communityId, String taskId, String userId) {

    Promise<JsonObject> promise = Promise.promise();
    this.searchState(communityId, taskId, userId, promise);
    return Model.fromFutureJsonObject(promise.future(), State.class);

  }

  /**
   * Search for the state with the specified identifier.
   *
   * @param communityId   identifier of the community in the state.
   * @param taskId        identifier of the task in the state.
   * @param userId        identifier of the user in the state.
   * @param searchHandler handler to manage the search.
   */
  void searchState(final String communityId, String taskId, String userId,
      Handler<AsyncResult<JsonObject>> searchHandler);

  /**
   * Store a state.
   *
   * @param state to store.
   *
   * @return the future stored state.
   */
  @GenIgnore
  default Future<State> storeState(final State state) {

    Promise<JsonObject> promise = Promise.promise();
    final var object = state.toJsonObject();
    if (object == null) {

      return Future.failedFuture("The state can not converted to JSON.");

    } else {

      this.storeState(object, promise);

    }

    return Model.fromFutureJsonObject(promise.future(), State.class);
  }

  /**
   * Store a state.
   *
   * @param state        to store.
   * @param storeHandler handler to manage the search.
   */
  void storeState(JsonObject state, Handler<AsyncResult<JsonObject>> storeHandler);

  /**
   * Update a state.
   *
   * @param state to update.
   *
   * @return the future update state.
   */
  @GenIgnore
  default Future<Void> updateState(final State state) {

    Promise<Void> promise = Promise.promise();
    final var object = state.toJsonObject();
    if (object == null) {

      return Future.failedFuture("The state can not converted to JSON.");

    } else {

      this.updateState(object, promise);

    }
    return promise.future();
  }

  /**
   * Update a state.
   *
   * @param state         to update.
   * @param updateHandler handler to manage the update result.
   */
  void updateState(JsonObject state, Handler<AsyncResult<Void>> updateHandler);

}
