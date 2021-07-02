/*
 * -----------------------------------------------------------------------------
 *
 * Copyright 2019 - 2022 UDT-IA, IIIA-CSIC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * -----------------------------------------------------------------------------
 */

package eu.internetofus.wenet_interaction_protocol_engine.persistence;

import eu.internetofus.common.vertx.Repository;
import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.mongo.FindOptions;
import io.vertx.ext.mongo.MongoClient;

/**
 * Implementation of the {@link NormsRepository}.
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class NormsRepositoryImpl extends Repository implements NormsRepository {

  /**
   * The name of the collection that contains the published norms.
   */
  public static final String PUBLISHED_NORMS_COLLECTION = "publishedNorms";

  /**
   * Create a new service.
   *
   * @param pool    to create the connections.
   * @param version of the schemas.
   */
  public NormsRepositoryImpl(final MongoClient pool, final String version) {

    super(pool, version);

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void searchPublishedNorm(final String id, final Handler<AsyncResult<JsonObject>> searchHandler) {

    final var query = new JsonObject().put("_id", id);
    this.findOneDocument(PUBLISHED_NORMS_COLLECTION, query, null, found -> {
      final var _id = (String) found.remove("_id");
      return found.put("id", _id);
    }).onComplete(searchHandler);

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void storePublishedNorm(final JsonObject norm, final Handler<AsyncResult<JsonObject>> storeHandler) {

    final var id = (String) norm.remove("id");
    if (id != null) {

      norm.put("_id", id);
    }
    this.storeOneDocument(PUBLISHED_NORMS_COLLECTION, norm, stored -> {

      final var _id = (String) stored.remove("_id");
      return stored.put("id", _id);

    }).onComplete(storeHandler);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void updatePublishedNorm(final JsonObject norm, final Handler<AsyncResult<Void>> updateHandler) {

    final var id = norm.remove("id");
    final var query = new JsonObject().put("_id", id);
    this.updateOneDocument(PUBLISHED_NORMS_COLLECTION, query, norm).onComplete(updateHandler);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void deletePublishedNorm(final String id, final Handler<AsyncResult<Void>> deleteHandler) {

    final var query = new JsonObject().put("_id", id);
    this.deleteOneDocument(PUBLISHED_NORMS_COLLECTION, query).onComplete(deleteHandler);

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void retrievePublishedNormsPage(final JsonObject query, final JsonObject sort, final int offset,
      final int limit, final Handler<AsyncResult<JsonObject>> searchHandler) {

    final var options = new FindOptions();
    options.setSort(sort);
    options.setSkip(offset);
    options.setLimit(limit);
    this.searchPageObject(PUBLISHED_NORMS_COLLECTION, query, options, "norms",
        norm -> norm.put("id", norm.remove("_id"))).onComplete(searchHandler);

  }

  /**
   * Migrate the collections to the current version.
   *
   * @return the future that will inform if the migration is a success or not.
   */
  public Future<Void> migrateDocumentsToCurrentVersions() {

    return this.updateSchemaVersionOnCollection(PUBLISHED_NORMS_COLLECTION);

  }

}
