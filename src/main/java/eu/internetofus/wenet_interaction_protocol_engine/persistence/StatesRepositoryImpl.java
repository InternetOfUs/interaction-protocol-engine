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

import eu.internetofus.common.components.interaction_protocol_engine.State;
import eu.internetofus.common.vertx.QueryBuilder;
import eu.internetofus.common.vertx.Repository;
import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.mongo.MongoClient;

/**
 * Implementation of the {@link StatesRepository}.
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class StatesRepositoryImpl extends Repository implements StatesRepository {

  /**
   * The name of the collection that contains the states.
   */
  public static final String STATES_COLLECTION = "states";

  /**
   * Create a new service.
   *
   * @param pool    to create the connections.
   * @param version of the schemas.
   */
  public StatesRepositoryImpl(final MongoClient pool, final String version) {

    super(pool, version);

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void searchState(final String communityId, String taskId, String userId,
      final Handler<AsyncResult<JsonObject>> searchHandler) {

    final var query = new QueryBuilder().withNoExistNullEqOrRegex("communityId", communityId)
        .withNoExistNullEqOrRegex("taskId", taskId).withNoExistNullEqOrRegex("userId", userId).build();
    this.findOneDocument(STATES_COLLECTION, query, new JsonObject().put("_id", false), null).onComplete(searchHandler);

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void storeState(final JsonObject state, final Handler<AsyncResult<JsonObject>> storeHandler) {

    this.storeOneDocument(STATES_COLLECTION, state, stored -> {

      stored.remove("_id");
      return stored;

    }).onComplete(storeHandler);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void updateState(final JsonObject state, final Handler<AsyncResult<Void>> updateHandler) {

    final var query = new QueryBuilder().withNoExistNullEqOrRegex("communityId", state.getString("communityId"))
        .withNoExistNullEqOrRegex("taskId", state.getString("taskId"))
        .withNoExistNullEqOrRegex("userId", state.getString("userId")).build();
    this.updateOneDocument(STATES_COLLECTION, query, state).onComplete(updateHandler);
  }

  /**
   * Migrate the collections to the current version.
   *
   * @return the future that will inform if the migration is a success or not.
   */
  public Future<Void> migrateDocumentsToCurrentVersions() {

    return this.migrateCollection(STATES_COLLECTION, State.class);

  }
}
